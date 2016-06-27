library(gplots)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

# for some reason, this line doesn't always work if running the whole script
folders = list.dirs("../data/trials3", recursive=F)
folders = folders[grepl("MT_Exp",folders)]


##
# Check ratings for letters correlate with overall ratings

finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)
finalLangs = finalLangs[!is.na(finalLangs$Word),]
finalLangs = finalLangs[nchar(finalLangs$Word)>0,]
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


processGeneration = function(folder){
    
    bits = strsplit(folder,"_")[[1]]
    chain = as.numeric(bits[4])
    gen = as.numeric(bits[5])
    
    condition =   "Comm"
    if(!"Comm" %in% bits){ 
      condition = "Learn"
      }
    
    resFile = list.files(folder, pattern='*.OUT')
    
    d = read.delim(paste(folder,"/", resFile,sep=''),sep='\t',quote="", stringsAsFactors = F, header=F)
    numNAs = apply(d,2,function(X){sum(is.na(X))})
    if(ncol(d)==35){
      d = d[,-10]
    }
    
    names(d) = c("spk",'state','round','date','t1','t2','t3','context','xx','meanings','inputLang','x1','t3','x2','x3','x4','t4','x5','x6','x7','x7b','contextSize','x8','x9','x10','x11','x12','x13', 'word','x15','target','x16','t5','x17')
      
    if(d[1,]$contextSize!=6){
      d[1,] = c(d[1,1:18],"-",d[1,19:33])
    }
    
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
    
    
    wordCounts = table(d$word)
    wordMeaningCounts = table(d$word,d$target)
    
    alldat = data.frame(condition = condition,
                        chain = finalLangChain,
                        gen = gen,
                        trial.nr = d$round,
                        context = d$context,
                        target.meaning = d$target,
                          word.produced = d$word)
    meaningsX = gsub("\\.png","",gsub("MonicaIconicity2012/","",stringListToString(d[1,]$meanings)))
    alldat$contextString = sapply(d$context,function(X){
      x = stringListToNum(X)+1
      paste(meaningsX[x],collapse=',')
    })
    alldat$targetString = meaningsX[alldat$target.meaning+1]
    
    
    res = data.frame(condition=rep(condition,sum(d$innovation)),chain=rep(chain,sum(d$innovation)),gen=rep(gen,sum(d$innovation)),round=rep(-1,sum(d$innovation)),parent=rep("",sum(d$innovation)),word=rep("",sum(d$innovation)),meaning=rep("",sum(d$innovation)),isSpikyMeaning=rep(F,sum(d$innovation)),increaseIconicity=rep(-1,sum(d$innovation)), wordCount=rep(-1,sum(d$innovation)) , wordCountSameMeaning=rep(-1,sum(d$innovation)), inFinalLang=rep(F,sum(d$innovation)), stringsAsFactors = F)
    
    
    currentParents = startingLang
    rcount = 1
    for(i in 1:nrow(d)){
      word = d[i,]$word
      meaning = d[i,]$target
      
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
        
        res[rcount,] = c(condition,chain,gen,i,parent,word,meaning,isSpikyMeaning,increaseIconicity,wordCounts[word], wordsUsedInSameMeaning, inFinalLang)
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


datax = data.frame()
alldatx = data.frame()
for(i in 1:length(folders)){
  print(folders[i])
  dx = processGeneration(folders[i])
  datax = rbind(datax,dx[[1]])
  alldatx = rbind(alldatx,dx[[2]])
}


alldatx = alldatx[order(alldatx$condition, alldatx$chain, alldatx$gen, alldatx$trial.nr),]

alldatx$estimatedSpikinessRating = getIconicity(as.character(alldatx$word.produced))

datax = datax[order(datax$condition,datax$chain, datax$gen, datax$round),]

write.csv(datax, file="../results/IncreaseInIconicity.csv")
write.csv(alldatx, file="../results/AllTrialData.csv")

hist(datax$increaseIconicity)
hist(datax[datax$condition=='Comm',]$increaseIconicity)
hist(datax[datax$condition=='Learn',]$increaseIconicity)

dens.comm = density(datax[datax$condition=='Comm',]$increaseIconicity)
dens.learn = density(datax[datax$condition=='Learn',]$increaseIconicity)

plot(dens.comm, main='', xlab='Change in iconicity')
lines(dens.learn, col=2)
abline(v=0)

pdf(file='../results/graphs/IncreaseIconcity_ConditionByMeaningIsSpiky.pdf')
plotmeans(increaseIconicity ~ paste(condition,isSpikyMeaning), data=datax, connect=list(1:2,3:4))
dev.off()

pdf(file='../results/graphs/IncreaseIconcity_ConditionByWordInFinalLanguage.pdf')
plotmeans(increaseIconicity ~ paste(condition,inFinalLang), data=datax, connect = list(1:2,3:4))
dev.off()




datax$condition = factor(datax$condition, levels=c("Learn","Comm"))
summary(lm(increaseIconicity ~ round+gen + condition*inFinalLang,data=datax))




plotmeans(increaseIconicity~gen, data=datax[datax$condition=='Comm' & datax$isSpikyMeaning==T & datax$inFinalLang,])
plotmeans(increaseIconicity~gen, data=datax[datax$condition=='Comm' & datax$isSpikyMeaning==F  & datax$inFinalLang,], add=T, col=2)
abline(h=0)

