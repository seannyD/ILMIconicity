library(gplots)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

# for some reason, this line doesn't always work if running the whole script
folders = list.dirs("../data/trials3", recursive=F)
folders = folders[grepl("MT_Exp",folders)]


##
# Check ratings for letters correlate with overall ratings

finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)
#finalLangs = finalLangs[!is.na(finalLangs$Word),]
#finalLangs = finalLangs[nchar(finalLangs$Word)>0,]
finalLangs[finalLangs$Cond=="Communication",]$Cond = "Comm"


items = finalLangs$Item[1:12]
finalLangs$meaningNum = match(finalLangs$Item,items)


finalLangs2 = finalLangs[!duplicated(finalLangs$Word),]

ratings = read.csv("../data/ratings/Sean_ratings.csv", stringsAsFactors = F)

ratings.letters = ratings[nchar(ratings$string)==1,]
rownames(ratings.letters) = ratings.letters$string
ratings.words = data.frame(string=finalLangs2$Word, rating = finalLangs2$RatedSpikiness, stringsAsFactors = F)



ratings.words$letterRating = sapply(ratings.words$string, function(X){
  mean(ratings.letters[strsplit(X,'')[[1]],]$rating)
})

ratings.words$length = sapply(ratings.words$string, nchar)

plot(ratings.words$rating,ratings.words$letterRating)
cor.test(ratings.words$rating,ratings.words$letterRating)

m0 = lm(rating~letterRating , data=ratings.words)
m1 = lm(rating~letterRating+length , data=ratings.words)
m2 = lm(rating~letterRating*length , data=ratings.words)
anova(m0,m1,m2)

chosenSpikinessModel = m2

summary(chosenSpikinessModel)


#####

getIconicity <- function(X){
  lr = mean(ratings.letters[strsplit(X,'')[[1]],]$rating, na.rm=T)
  predict(chosenSpikinessModel,newdata=data.frame(letterRating=lr,length=nchar(X)))
}

####

cleanWords = function(x){
  gsub("ç",'c',x)
}

stringListToString = function(x){
  startingLang = cleanWords(gsub(" ","",x))
  startingLang = gsub("\\[","",startingLang)
  startingLang = gsub("\\]","",startingLang)
  startingLang = strsplit(startingLang, ',')[[1]]
}


stringListToNum = function(x){
  startingLang = gsub("\\[","",x)
  startingLang = gsub("\\]","",startingLang)
  startingLang = as.numeric(strsplit(startingLang, ',')[[1]])
}

load_out_file = function(f){
  dx = read.delim(f,sep='\t',quote="", stringsAsFactors = F, header=T)
  dx = dx[,names(dx)!='CurPartRole']
  return(dx)
}

getByPartResFiles = function(folder){
  resFiles = list.files(folder, pattern='*.out')
  
  dx = load_out_file(paste(folder,"/", resFiles[1],sep=''))
  if(!grepl("Learn",folder)){
    dx2 = load_out_file(paste(folder,"/", resFiles[2],sep=''))
    dx = rbind(dx1,dx2)
  }
  dx = dx[dx$ListenersResponse!="-",]
  dx = dx[order(dx$StimNum),]
  dx$ListenersResponse = as.numeric(dx$ListenersResponse)
  dx$correct = dx$Stimulus == dx$ListenersResponse
  dx$correctSpikiness = (dx$Stimulus < 6) == (dx$ListenersResponse < 6)
  return(dx)
}

processGeneration = function(folder){
    
    bits = strsplit(folder,"_")[[1]]
    chain = as.numeric(bits[4])
    gen = as.numeric(bits[5])
    
    condition =   "Comm"
    if(!"Comm" %in% bits){ 
      condition = "Learn"
      }
    
    
    
#    resFile = list.files(folder, pattern='*.OUT')
#    d = read.delim(paste(folder,"/", resFile,sep=''),sep='\t',quote="", stringsAsFactors = F, header=F)
#    numNAs = apply(d,2,function(X){sum(is.na(X))})
#    if(ncol(d)==35){
#      d = d[,-10]
#    }

#    if(d[1,]$contextSize!=6){
#      d[1,] = c(d[1,1:18],"-",d[1,19:33])
#    }
    
    d = getByPartResFiles(folder)    
    names(d) = c("spk",'state','round','Response','date','AIWait','BetweenWait','context','contextSet','meanings','inputLang','Feedback','FeedbackWait','FinalTest','ForSending','Human','ImageWait','ListenersChoicesType','ListenersResponse','Message','Network','contextSize','PartBreak','ResetScore','RoleSwitch','SendSignals','SpeakerSeesContext','SpeakersChoicesType', 'word','SpeakersTargetType','target','Training','WordWait','XXListenersResponse','X','correct','correctSpikiness')
    
    

    d$word = cleanWords(d$word)
    
    startingLang = stringListToString(d[1,]$inputLang)
    
    finalLangChain = chain - 1
    if(chain>2){ finalLangChain = chain-2}
    if(condition=="Learn"){
      finalLangChain = chain+3
    }
    
    finalLang = finalLangs[finalLangs$Cond==condition & finalLangs$Chain==finalLangChain & finalLangs$Gen==gen,]$Word
    
    # identify innovations
    d$innovation = !(d$word %in% startingLang  | duplicated(d$word))
    d$innovationForMeaning = ! d$word == startingLang[d$target+1]
      
    
    
    wordCounts = table(d$word)
    wordMeaningCounts = table(d$word,d$target)
    
    alldat = data.frame(condition = condition,
                        chain = finalLangChain,
                        gen = gen,
                        trial.nr = d$round,
                        context = d$context,
                        target.meaning = d$target,
                        word.produced = d$word,
                        innovation = d$innovation,
                        innovationForMeaning = d$innovationForMeaning,
                        guessedMeaning = d$ListenersResponse,
                        correctGuess = d$correct,
                        correctSpikiness = d$correctSpikiness)
    meaningsX = gsub("\\.png","",gsub("MonicaIconicity2012/","",stringListToString(d[1,]$meanings)))
    alldat$contextString = sapply(d$context,function(X){
      x = stringListToNum(X)+1
      paste(meaningsX[x],collapse=',')
    })
    alldat$targetString = meaningsX[alldat$target.meaning+1]
    
    
    res = data.frame(
      condition=rep(condition,sum(d$innovation)),
      chain=rep(chain,sum(d$innovation)),
      gen=rep(gen,sum(d$innovation)),
      round=rep(-1,sum(d$innovation)),
      parent=rep("",sum(d$innovation)),
      word=rep("",sum(d$innovation)),
      meaning=rep("",sum(d$innovation)),
      isSpikyMeaning=rep(F,sum(d$innovation)),
      increaseIconicity=rep(-1,sum(d$innovation)), 
      wordCount=rep(-1,sum(d$innovation)) , 
      wordCountSameMeaning=rep(-1,sum(d$innovation)), 
      inFinalLang=rep(F,sum(d$innovation)), 
      guessedMeaning = rep(F,sum(d$innovation)), 
      correctGuess = rep(F,sum(d$innovation)), 
      correctSpikiness = rep(F,sum(d$innovation)), 
    stringsAsFactors = F)
    
    
    currentParents = startingLang
    rcount = 1
    for(i in 1:nrow(d)){
      word = d[i,]$word
      meaning = d[i,]$target
      guessedMeaning = d[i,]$ListenersResponse
      correctGuess = d[i,]$correct
      correctSpikiness = d[i,]$correctSpikiness
      if(d[i,]$innovation){
        parent = currentParents[meaning+1]
        
        oldIconicity = getIconicity(parent)
        newIconicity = getIconicity(word)
        
        # If it's a spiky word, then we predict spikiness rating should go up
        #  if so, new iconicity > old iconicity, so this gives positive number
        increaseIconicity = newIconicity - oldIconicity
        
        # but if it's not a spiky meaning, we predict the rating should go down
        isSpikyMeaning = meaning<=5
        if(!isSpikyMeaning){
          increaseIconicity = oldIconicity - newIconicity
        }
        
        wordsUsedInSameMeaning = wordMeaningCounts[word, meaning+1]
        
        inFinalLang = word == finalLang[meaning+1]
        if(is.na(inFinalLang)){
          xxxx
        }
        
        res[rcount,] = c(condition,
                         chain,
                         gen,
                         i,
                         parent,
                         word,
                         meaning,
                         isSpikyMeaning,
                         increaseIconicity,
                         wordCounts[word], 
                         wordsUsedInSameMeaning, 
                         inFinalLang,
                         guessedMeaning,
                         correctGuess,
                         correctSpikiness
                        )
        rcount = rcount + 1
      }
      currentParents[meaning+1] = word
    }
    
    for(x in c("increaseIconicity",'wordCount','wordCountSameMeaning','round','gen')){
      res[,x] = as.numeric(res[,x])
    }
    res$inFinalLang = res$inFinalLang=="TRUE"
    return(list(res,alldat))
}

#######################
# go through all the folders and extract data

datax = data.frame()
alldatx = data.frame()
for(i in 1:length(folders)){
  print(folders[i])
  dx = processGeneration(folders[i])
  datax = rbind(datax,dx[[1]])
  alldatx = rbind(alldatx,dx[[2]])
}

table(alldatx$chain,alldatx$gen,alldatx$condition)


alldatx = alldatx[order(alldatx$condition, alldatx$chain, alldatx$gen, alldatx$trial.nr),]

alldatx$estimatedSpikinessRating = getIconicity(as.character(alldatx$word.produced))

datax = datax[order(datax$condition,datax$chain, datax$gen, datax$round),]

write.csv(datax, file="../results/IncreaseInIconicity.csv")
write.csv(alldatx, file="../results/AllTrialData.csv")




plotChainRes = function(ret){
  arin =which(!is.na(ret),arr.ind=T)
  plot(c(0.5,4.5),c(12.5,0.5),type='n',xlab='',ylab='',xaxt='n',yaxt='n',ylim=c(12.5,0.5))
  
  
  for(i in 1:3){
    from = ret[,i]
    to = ret[,(i+1):4]
    for(j in 1:12){
      tx = which(to==from[j],arr.ind=T)
      if(nrow(tx)>0){
      for(z in 1:nrow(tx)){
        lines(c(i,tx[z,2]+1),c(j,tx[z,1]), col='gray')
      }
      }
    }
  }
  text(arin[,2],arin[,1],matrix(ret,ncol=1), col = c("black",'red')[grepl('\\*',ret)+1])
  
}


alldatx$word.produced = as.character(alldatx$word.produced)

for(cond in unique(alldatx$condition)){
  dx = alldatx[alldatx$condition==cond,]
  for(chain in sort(unique(dx$chain))){
    chainRes = data.frame(X=1:12)
    dxx = dx[dx$chain==chain,]
    for(gen in sort(unique(dxx$gen))){
      dxxx = dxx[dxx$gen==gen,]
      dxxx = dxxx[order(dxxx$target.meaning),]
      
      dxxx$trial.nr = dxxx$trial.nr - min(dxxx$trial.nr)
      dxxx$var = paste(
        dxxx$word.produced,
       # c("","*")[dxxx$innovationForMeaning+1])
       c("","*")[dxxx$innovation+1])
      p1_1 = dxxx[dxxx$trial.nr %in% seq(0,0+23,2),]$var
      p2_1 = dxxx[dxxx$trial.nr %in% seq(1,0+24,2),]$var
      p1_2 = dxxx[dxxx$trial.nr %in% seq(24,24+23,2),]$var
      p2_2 = dxxx[dxxx$trial.nr %in% seq(25,24+23,2),]$var
      
      ret = cbind(p1_1,p2_1,p1_2,p2_2,b=rep("",length(p1_1)))
      colnames(ret) = c(paste(chain,gen,c("p1_1","p2_1","p1_2","p2_2")),"")
      #plotChainRes(ret)
      
      if(nrow(ret)==12){
      chainRes = cbind(chainRes,ret)
      }
    }
    write.csv(chainRes,paste("../results/chainRes/",cond,'_',chain,'.csv',sep=''))
  }
}
