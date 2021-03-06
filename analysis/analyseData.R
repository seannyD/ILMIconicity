library(gplots)
library(vegan)
rm(list=ls())
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

meaningDistance = 
  outer(finalLangs$Shape[1:12],finalLangs$Shape[1:12],"!=") +
  outer(finalLangs$Colour[1:12],finalLangs$Colour[1:12],"!=") +
  outer(finalLangs$Border[1:12],finalLangs$Border[1:12],"!=")




finalLangs2 = finalLangs[!duplicated(finalLangs$Word),]

# load model to predict spikiness
load('PredictSpikinessModel.RDat')

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
    dx = rbind(dx,dx2)
  }
  dx = dx[dx$ListenersResponse!="-",]
  dx = dx[order(dx$StimNum),]
  
  
  dx$ListenersResponse = as.numeric(dx$ListenersResponse)
  dx$correct = dx$Stimulus == dx$ListenersResponse
  dx$correctSpikiness = (dx$Stimulus < 6) == (dx$ListenersResponse < 6)
  
  if(grepl("Learn",folder)){
  startLangX = stringListToString(dx$CurrentSignals[1])
  
  humanT = table(dx$SpeakersResponse %in% startLangX,rep(1:2,length.out=nrow(dx)))
  print(humanT)
  human = 1
  if(nrow(humanT)!=1){
  
    human = which(humanT[1,]!=0)
  }
  print(c("Human",human))
  dx$Human = rep(c(human==1,human!=1),length.out=nrow(dx))
  
  } else{
    dx$Human = T
  }
  
  return(dx)
}

getSystematicity = function(lang){
  signalDist = adist(lang)
  a = matrix(rep(nchar(lang),length(lang)),ncol=length(lang))
  b = matrix(rep(nchar(lang),length(lang)),ncol=length(lang),byrow=T)
  a[b>a] = b[b>a]
  # normalise signal dist
  signalDist = signalDist/a
  ret = vegan::mantel(signalDist,meaningDistance,permutations=1,method='spearman')$statistic
  return(ret)
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
    d$innovation.mutation = !(d$word %in% startingLang  | duplicated(d$word))
    d$innovationForMeaning = ! d$word == startingLang[d$target+1]
     
    d$innovation  = F 
    currWords = startingLang
    for(i in 1:nrow(d)){
      if(d[i,]$word != currWords[d[i,]$target+1]){
        d[i,]$innovation = T
        currWords[d[i,]$target+1] = d[i,]$word
      }
    }
    
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
                        innovation.mutation = d$innovation.mutation,
                        guessedMeaning = d$ListenersResponse,
                        correctGuess = d$correct,
                        correctSpikiness = d$correctSpikiness,
                        Human = d$Human)
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
      systematicity.increase = rep(F,sum(d$innovation)),
      innovation.mutation = rep(F,sum(d$innovation)),
      Human = d[d$innovation,]$Human,
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
        
        oldIconicity = getIconicityFromRForest(parent)
        newIconicity = getIconicityFromRForest(word)
        
        # If it's a spiky word, then we predict spikiness rating should go up
        #  if so, new iconicity > old iconicity, so this gives positive number
        increaseIconicity = newIconicity - oldIconicity
        
        # but if it's not a spiky meaning, we predict the rating should go down
        isSpikyMeaning = meaning<=5
        if(!isSpikyMeaning){
          increaseIconicity = oldIconicity - newIconicity
        }

        oldSystematicity = getSystematicity(currentParents)
        curp2 = currentParents
        curp2[meaning+1] = word
        newSystematicity = getSystematicity(curp2)
        
        # systematicity should incrase
        sys.diff = newSystematicity - oldSystematicity
        
                
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
                         correctSpikiness,
                         sys.diff,
                         d[i,]$innovation.mutation,
                         d[i,]$Human
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

alldatx$estimatedSpikinessRating = getIconicityFromRForest(as.character(alldatx$word.produced))


datax = datax[order(datax$condition,datax$chain, datax$gen, datax$round),]

write.csv(datax, file="../results/IncreaseInIconicity.csv")
write.csv(alldatx, file="../results/AllTrialData.csv", row.names = F)




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


# Numberof unique words in each condition:
tapply(alldatx[alldatx$Human,]$word.produced, alldatx[alldatx$Human,]$condition, function(X){length(unique(X))})

alldatx$pair = paste(alldatx$condition, alldatx$chain, alldatx$gen)

commUnique = tapply(alldatx[alldatx$Human & alldatx$condition=='Comm',]$word.produced,
       alldatx[alldatx$Human & alldatx$condition=='Comm',]$pair, 
       function(X){length(unique(X))})

learnUnique = tapply(alldatx[alldatx$Human & alldatx$condition=='Learn',]$word.produced,
                    alldatx[alldatx$Human & alldatx$condition=='Learn',]$pair, 
                    function(X){length(unique(X))})

t.test(commUnique,learnUnique)




