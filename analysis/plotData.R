library(gplots)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

datax = read.csv("../results/IncreaseInIconicity.csv", stringsAsFactors = F)
alldatx = read.csv("../results/AllTrialData.csv", stringsAsFactors = F)

hist(datax$increaseIconicity)
hist(datax[datax$condition=='Comm',]$increaseIconicity)
hist(datax[datax$condition=='Learn',]$increaseIconicity)


comm.innovation.iconicity.dist = datax[datax$condition=='Comm' & datax$Human,]$increaseIconicity
learn.innovation.iconicity.dist = datax[datax$condition=='Learn' & datax$Human,]$increaseIconicity

wilcox.test(comm.innovation.iconicity.dist)
wilcox.test(learn.innovation.iconicity.dist)

symmetry.test(comm.innovation.iconicity.dist)
symmetry.test(learn.innovation.iconicity.dist)


dens.comm = density(datax[datax$condition=='Comm' & datax$Human,]$increaseIconicity, adjust=1)
dens.learn = density(datax[datax$condition=='Learn' & datax$Human,]$increaseIconicity,adjust=1)

plot(dens.comm, main='', xlab='Change in iconicity')
lines(dens.learn, col=2)
abline(v=0)

pdf(file='../results/graphs/IncreaseIconcity_ConditionByMeaningIsSpiky.pdf')
plotmeans(increaseIconicity ~ paste(condition,isSpikyMeaning), data=datax, connect=list(1:2,3:4))
dev.off()

pdf(file='../results/graphs/IncreaseIconcity_andSystematicity_ConditionByWordInFinalLanguage.pdf', width=10, height=4)
par(mfrow=c(1,2))
plotmeans(increaseIconicity ~ paste(condition,inFinalLang), data=datax[datax$Human,], connect = list(1:2,3:4),xlab='', ylab="Increase", legends = c("Rejected","Survived","Rejected","Survived"))
title(main="Iconicity")
axis(1,at=c(1.5,3.5),c("Communication","Reproduction"),line=1, tick=F)
abline(h=0)
plotmeans(systematicity.increase ~ paste(condition, inFinalLang), data = datax[datax$Human,], connect = list(1:2,3:4), xlab='', ylab="Increase",, legends = c("Rejected","Survived","Rejected","Survived"))
title(main="Systematicity")
axis(1,at=c(1.5,3.5),c("Communication","Reproduction"),line=1, tick=F)
abline(h=0)
dev.off()



plotmeans(correctGuess~gen,alldatx[alldatx$condition=='Learn' & !alldatx$Human,], n.label = F)
plotmeans(correctGuess~gen,alldatx[alldatx$condition=='Comm',],add=T,col=2,barcol=2, n.label = F)

ctrl = glmerControl(optCtrl = list(maxfun=50000))

m0 = glmer(correctGuess ~ 1 + (1|chain) + (1|target.meaning), data=alldatx[alldatx$condition=='Comm' | (!alldatx$Human),], family = binomial, control= ctrl)
m1 = glmer(correctGuess ~ condition + (1|chain) + (1|target.meaning), data=alldatx[alldatx$condition=='Comm' | (!alldatx$Human),], family = binomial, control= ctrl)
anova(m0,m1)
getMEText(anova(m0,m1),ef = "main effect of condition", summary(m1)$coef[2,])


plotmeans(correctGuess~gen,alldatx[alldatx$condition=='Learn' & alldatx$trial.nr >102 & !alldatx$Human,])
plotmeans(correctGuess~gen,alldatx[alldatx$condition=='Comm' & alldatx$trial.nr >102,],add=T,col=2,barcol=2)


plotmeans(correctSpikiness~gen,alldatx[alldatx$condition=='Comm',])
plotmeans(correctSpikiness~gen,alldatx[alldatx$condition=='Learn' & !alldatx$Human,],add=T,col=2,barcol=2)


###########

library(ggplot2)
p <- ggplot(datax, aes(factor(condition):factor(inFinalLang), increaseIconicity, fill=condition))
p + geom_violin() + stat_summary(fun.y=mean, geom="point", size=4, color="red")

p + geom_violin() + geom_boxplot(width=0.1) +
  theme(text=element_text(size=20), legend.position="none") +
  scale_y_continuous(name="Score ranking")+
  scale_x_discrete(name="")+
  scale_fill_grey(start = 0.55, end=0.8)


p <- ggplot(datax, aes(factor(condition):factor(inFinalLang), systematicity.increase, fill=condition))
p + geom_violin() + stat_summary(fun.y=mean, geom="point", size=4, color="red")

p + geom_violin() + geom_boxplot(width=0.1) +
  theme(text=element_text(size=20), legend.position="none") +
  scale_y_continuous(name="Score ranking")+
  scale_x_discrete(name="")+
  scale_fill_grey(start = 0.55, end=0.8)


###########





datax$condition = factor(datax$condition, levels=c("Learn","Comm"))
summary(lm(increaseIconicity ~ round+gen + condition*inFinalLang,data=datax))




plotmeans(increaseIconicity~gen, data=datax[datax$condition=='Comm' & datax$isSpikyMeaning==T & datax$inFinalLang,])
plotmeans(increaseIconicity~gen, data=datax[datax$condition=='Comm' & datax$isSpikyMeaning==F  & datax$inFinalLang,], add=T, col=2)
abline(h=0)


plotmeans(increaseIconicity~wordCountSameMeaning ,data=datax[datax$wordCountSameMeaning<4,])
abline(h=0)

summary(lm(increaseIconicity~wordCountSameMeaning*condition, data=datax[datax$wordCountSameMeaning<4,]))


plotmeans(increaseIconicity~cut(round,4), datax[datax$condition=='Learn',], ylim=c(-0.15,0.15))
plotmeans(increaseIconicity~cut(round,4), datax[datax$condition=='Comm',], ylim=c(-0.15,0.15),add=T, col=2, barcol = 2)


plotmeans(systematicity.increase~cut(round,4), datax[datax$condition=='Learn',])
plotmeans(systematicity.increase~cut(round,4), datax[datax$condition=='Comm',],add=T, col=2, barcol = 2)

pdf("../results/graphs/CorrectGuesses_byIncreaseIconicity.pdf", width=8, height=4)
par(mfrow=c(1,2))
ylimx = c(-0.045,0.045)
plotmeans(increaseIconicity ~ paste(condition, correctGuess), data = datax[datax$Human & datax$condition=="Comm",], ylim=ylimx, legends = c("Incorrect","Correct"),xlab='',ylab="Increase in iconicity")
title("Guessing Item")
abline(h=0)
plotmeans(increaseIconicity ~ paste(condition, correctSpikiness), data = datax[datax$Human& datax$condition=="Comm",], ylim=ylimx,legends = c("Incorrect","Correct"),xlab='',ylab="Increase in iconicity")
title("Guessing Shape")
abline(h=0)
dev.off()

m0 = lmer(increaseIconicity ~ 1 + (1|chain) + (1|gen), data=datax[datax$Human & datax$condition=="Comm",])

m1 = lmer(increaseIconicity ~ correctGuess + (1|chain) + (1|gen), data=datax[datax$Human & datax$condition=="Comm",])

m2 = lmer(increaseIconicity ~ correctGuess + correctSpikiness + (1|chain) + (1|gen), data=datax[datax$Human & datax$condition=="Comm",])

anova(m0,m1,m2)

summary(m2)


dens.commS = density(datax[datax$condition=='Comm',]$systematicity.increase)
dens.learnS = density(datax[datax$condition=='Learn',]$systematicity.increase)

plot(dens.commS, main='', xlab='Change in iconicity')
lines(dens.learnS, col=2)
abline(v=0)


cx = ctree(RatedSpikiness~as.factor(Cond)+as.factor(Gen) + as.factor(Shape) + as.factor(Colour) + as.factor(Border), data=finalLangs)
plot(cx)




plot(datax$increaseIconicity,datax$systematicity.increase)
cor.test(datax$increaseIconicity,datax$systematicity.increase)

plot(datax[datax$condition=="Comm",]$increaseIconicity,datax[datax$condition=="Comm",]$systematicity.increase)
cor.test(datax[datax$condition=="Comm",]$increaseIconicity,datax[datax$condition=="Comm",]$systematicity.increase)

plot(datax[datax$condition=="Learn" & datax$Human,]$increaseIconicity,datax[datax$condition=="Learn" & datax$Human,]$systematicity.increase)
cor.test(datax[datax$condition=="Learn" & datax$Human,]$increaseIconicity,datax[datax$condition=="Learn" & datax$Human,]$systematicity.increase)

m0 = lmer(increaseIconicity~condition*gen + (1|chain) + (1|meaning), data = datax[datax$Human,])
m1 = lmer(increaseIconicity~systematicity.increase+(condition*gen) + (1|chain) + (1|meaning), data = datax[datax$Human,])
m2 = lmer(increaseIconicity~systematicity.increase + (systematicity.increase:condition) + (condition*gen) + (1|chain) + (1|meaning), data = datax[datax$Human,])
m3 = lmer(increaseIconicity~systematicity.increase*condition*gen + (1|chain) + (1|meaning), data = datax[datax$Human,])
anova(m0,m1,m2,m3)

sjp.lmer(m2,type='fe')
