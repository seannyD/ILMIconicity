library(gplots)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

datax = read.csv("../results/IncreaseInIconicity.csv", stringsAsFactors = F)
alldatx = read.csv("../results/AllTrialData.csv", stringsAsFactors = F)

hist(datax$increaseIconicity)
hist(datax[datax$condition=='Comm',]$increaseIconicity)
hist(datax[datax$condition=='Learn',]$increaseIconicity)

dens.comm = density(datax[datax$condition=='Comm',]$increaseIconicity)
dens.learn = density(datax[datax$condition=='Learn',]$increaseIconicity)

plot(dens.learn, main='', xlab='Change in iconicity')
lines(dens.comm, col=2)
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


plotmeans(increaseIconicity~wordCountSameMeaning ,data=datax[datax$round<24 & datax$condition=='Comm',])
abline(h=0)
summary(lm(increaseIconicity~wordCountSameMeaning*condition, data=datax[datax$round<24,]))


plotmeans(increaseIconicity~cut(round,6), datax[datax$condition=='Learn',])
plotmeans(increaseIconicity~cut(round,6), datax[datax$condition=='Comm',])


plotmeans(increaseIconicity ~ paste(condition, correctGuess), data = datax)
