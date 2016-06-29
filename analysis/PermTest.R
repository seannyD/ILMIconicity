library(gplots)
library(car)
library(lattice)
library(lme4)
library(effects)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)

#finalLangs = finalLangs[finalLangs$Colour == "Rojo",]

finalLangs$cond2 = paste(finalLangs$Cond,finalLangs$Chain, finalLangs$Gen)

split = finalLangs[finalLangs$cond2==unique(finalLangs$cond2)[1],]$Shape



res = tapply(finalLangs$RatedSpikiness, finalLangs$cond2, function(X){
  trueDiff = -diff(tapply(X, split,mean))
  permDiff = replicate(10000,
        {-diff(tapply(sample(X), split,mean)) })
  p = sum(trueDiff > permDiff ) / length(permDiff)
  z.score = (trueDiff - mean(permDiff)) / sd(permDiff)
  return(c(p,z.score))
})

res2 = data.frame(
p = sapply(res,head,n=1),
z = sapply(res,tail,n=1),
t(sapply(names(res),function(X){
  strsplit(X," ")[[1]]
}))
, stringsAsFactors = F)
names(res2) = c("p",'z','condition','chain','gen')

plot(c(1,6), c(-3,3), type='n',ylab='Z score')
for(i in unique(res2$chain)){
  dx =res2[res2$chain==i,]
  lines(dx$gen,dx$z, col=c("black","red")[(dx$condition=="Learn")+1])
}

plot(c(1,6), c(0,1), type='n',ylab='p')
for(i in unique(res2$chain)){
  dx =res2[res2$chain==i,]
  lines(dx$gen,dx$p, col=c("black","red")[(dx$condition=="Learn")+1])
}

######

split = finalLangs[finalLangs$cond2==unique(finalLangs$cond2)[1],]$Shape
permWithin = finalLangs[finalLangs$cond2==unique(finalLangs$cond2)[1],]$Colour


res3 = tapply(finalLangs$RatedSpikiness, finalLangs$cond2, function(X){
  trueDiff = -diff(tapply(X, split,mean))
  permDiff = replicate(10000,
                       {-diff(tapply(
                         unlist(tapply(X,permWithin,sample)), split,mean)) })
  p = sum(trueDiff > permDiff ) / length(permDiff)
  z.score = (trueDiff - mean(permDiff)) / sd(permDiff)
  return(c(p,z.score))
})

res4 = data.frame(
  p = sapply(res3,head,n=1),
  z = sapply(res3,tail,n=1),
  t(sapply(names(res3),function(X){
    strsplit(X," ")[[1]]
  }))
  , stringsAsFactors = F)
names(res4) = c("p",'z','condition','chain','gen')

plot(c(1,6), c(-20,20), type='n',ylab='Z score')
for(i in unique(res4$chain)){
  dx =res4[res4$chain==i,]
  lines(dx$gen,dx$z, col=c("black","red")[(dx$condition=="Learn")+1])
}

plot(c(1,6), c(0,1), type='n',ylab='p')
for(i in unique(res4$chain)){
  dx =res4[res4$chain==i,]
  lines(dx$gen,dx$p, col=c("black","red")[(dx$condition=="Learn")+1])
}

