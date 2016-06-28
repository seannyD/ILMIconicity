library(ngram)
library(gplots)
library(lme4)
library(party)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

predictSpikinessWithLMER = function(ratings.words){
  m.words = lmer(RatingSpikiness~ Item + (1|Part), data=ratings.words)
  #plot(ratings.words$RatingSpikiness,resid(m.words))
  words = sort(unique(ratings.words$Item))
  words.predictions = predict(m.words,newdata=data.frame(Item=words, Part=1), re.form=NULL)
  names(words.predictions) = words
  #cor(words.predictions, tapply(ratings.words$RatingSpikiness, ratings.words$Item, mean))
  return(words.predictions)
}


ratings = read.delim("../data/ratings/SpikinessRatings", sep='\t', stringsAsFactors = F)

m0 = lmer(RatingSpikiness ~ Sex + Age + Likert + (1|Item)  + (1|Part), data=ratings)
summary(m0)


ratings.letters = ratings[nchar(ratings$Item)==1,]
ratings.words = ratings[nchar(ratings$Item)>1,]


letter.predictions = predictSpikinessWithLMER(ratings.letters)
words.predictions = predictSpikinessWithLMER(ratings.words)

letterRaingsOfWords = sapply(words, function(X){
  mean(letter.predictions[strsplit(X,'')[[1]]])
})

plot(letterRaingsOfWords, words.predictions)

# Baseline for just using letters:
cor(letterRaingsOfWords, words.predictions)



###############

makeFeatureFrame = function(dx,ngrams){
  r = matrix(nrow=nrow(dx), ncol=2+length(ngrams))
  r[,1] = dx$Item
  r[,2] = dx$RatingSpikiness
  colnames(r) = c("Item","RatingSpikiness",ngrams)
  for(i in 3:ncol(r)){
    r[,i] = grepl(colnames(r)[i],r[,1])
  }
  r = as.data.frame(r)
  for(i in 3:ncol(r)){
    r[,i] = as.logical(r[,i])
  }
  return(r)
}


proportionOfDataInTrainingSet = 0.75
numberOfFolds = 20
maxNGram = 2

set.seed(2189)
res = c()

for(run in 1:numberOfFolds){
  items = unique(ratings$Item)
  trainItems = c(items[nchar(items)==1],sample(items[nchar(items)>1],sum(nchar(items)>1)*proportionOfDataInTrainingSet))
  testItems = items[!items %in% trainItems]
  
  trainSet = ratings[ratings$Item %in% trainItems,]
  testSet = ratings[ratings$Item %in% testItems,]
  
  
  ngrams = unique(unlist(sapply(trainSet$Item, function(X){
    if(nchar(X)==1){
      return(X)
    }
    unique(ngram_asweka(X,min=1,max=maxNGram,sep=''))
  })))
  
  rTrain = makeFeatureFrame(trainSet,ngrams)
  rTrain$RatingSpikiness = as.numeric(rTrain$RatingSpikiness)
  
  rTest.predictions = predictSpikinessWithLMER(testSet)
  rTest = makeFeatureFrame(testSet[!duplicated(testSet$Item),],ngrams)
  rTest$RatingSpikiness = rTest.predictions[rTest$Item]
  
  colselect = 2:ncol(rTrain)
  #tx =ctree(RatingSpikiness ~ ., data=rTrain[,colselect])
  #plot(tx)
  
  cf = cforest(RatingSpikiness ~ . , 
               data= rTrain[,colselect], 
               controls = cforest_control(mtry = 10))
  #importance=  varimp(cf)
  #dotplot(sort(importance))
  
  predictedRatings = predict(cf,newdata=rTest[,colselect])
  
  #plot(predictedRatings,rTest$RatingSpikiness)
  #cor.test(predictedRatings,rTest$RatingSpikiness)
  res = c(res,cor(predictedRatings,rTest$RatingSpikiness))
}

mean(res)
#2grams = 0.886473

# make model with whole data


ngrams.all = unique(unlist(sapply(ratings$Item, function(X){
  if(nchar(X)==1){
    return(X)
  }
  unique(ngram_asweka(X,min=1,max=maxNGram,sep=''))
})))

rAll = makeFeatureFrame(ratings,ngrams.all)
rAll$RatingSpikiness = as.numeric(rAll$RatingSpikiness)

rAll.predictions = predictSpikinessWithLMER(ratings)
rAll = makeFeatureFrame(ratings[!duplicated(ratings$Item),],ngrams.all)
rAll$RatingSpikiness = rAll.predictions[rAll$Item]

cf.all = cforest(RatingSpikiness ~ . , 
             data= rAll[,2:ncol(rAll)], 
             controls = cforest_control(mtry = 10))


getIconicityFromRForest = function(word){
  newData = c(Item=NA,RatingSpikiness=NA,sapply(ngrams.all, function(X){grepl(X,word)}))
  newData = as.data.frame(t(newData))
  predictedRatings = predict(cf.all,newdata=newData)
  return(predictedRatings)
}

getIconicityFromRForest = function(words){
  
  xdat = t(sapply(words,function(word){sapply(ngrams.all, function(X){grepl(X,word)})}))
  xdat = as.data.frame(cbind(rep(NA,nrow(xdat)),rep(NA,nrow(xdat)),xdat))
  
  predictedRatings = predict(cf.all,newdata=xdat)
  return(as.vector(predictedRatings))
}


save(getIconicityFromRForest, ngrams.all, cf.all, file='PredictSpikinessModel.RDat')
