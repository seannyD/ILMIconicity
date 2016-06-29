library(gplots)
library(car)
library(lattice)
library(lme4)
library(effects)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)

finalLangs$RatedSpikiness.center = finalLangs$RatedSpikiness- mean(finalLangs$RatedSpikiness)

finalLangs$Cond = factor(finalLangs$Cond, levels=c("Learn","Communication"))
finalLangs$Shape = factor(finalLangs$Shape, levels=c("Redondo","Picudo"))

par(mar=c(8,4,2,2))
plotmeans(finalLangs$RatedSpikiness.center~finalLangs$Item, las=2, xlab=F)

summary(aov(RatedSpikiness.center ~ Cond * Gen * Shape , data=finalLangs))

summary(aov(RatedSpikiness.center ~ Gen * Shape , data=finalLangs[finalLangs$Cond=="Learn",]))

m0 = lmer(RatedSpikiness.center ~ 1 + (1 |Chain) + (1|Gen) , data=finalLangs)
m0B = lmer(RatedSpikiness.center ~ 1 + (1 |Chain) + (1|Gen) + (1|Item) , data=finalLangs)

anova(m0,m0B)

m0 = lmer(RatedSpikiness.center ~ 1 + (1 |Chain)  + (1|Item), data=finalLangs)
m1 = lmer(RatedSpikiness.center ~ Cond + (1 |Chain)  + (1|Item), data=finalLangs)
m2 = lmer(RatedSpikiness.center ~ Cond + Gen + (1 |Chain) + (1|Item), data=finalLangs)
m3 = lmer(RatedSpikiness.center ~ Cond + Gen + Shape + (1 |Chain)  + (1|Item), data=finalLangs)
m4 = lmer(RatedSpikiness.center ~ Cond + (Gen * Shape) + (1 |Chain)  + (1|Item), data=finalLangs)
m5 = lmer(RatedSpikiness.center ~ (Cond*Gen) + (Gen * Shape) + (1 |Chain)  + (1|Item), data=finalLangs)
m6 = lmer(RatedSpikiness.center ~ (Cond*Gen) + (Gen * Shape) + (Shape:Cond) + (1 |Chain)  + (1|Item), data=finalLangs)
m7 = lmer(RatedSpikiness.center ~ Cond * Gen * Shape + (1 |Chain)  + (1|Item), data=finalLangs)

anova(m0,m1,m2,m3,m4,m5,m6,m7)

dotplot(ranef(m7))
dotplot(fixef(m7))


fe = fixef(m7)
stderr = summary(m7)$coefficients[,2]
par(mar=c(4,17,2,2))
plot(1:length(fixef(m7))~fixef(m7), pch=16, xlim=c(-0.35,0.35),ylim=c(length(fe),1), xlab='Fixed effect', ylab='', yaxt='n')
axis(2,at=1:8, labels=names(fe), las=2)
abline(v=0)
for(i in 1:length(fe)){
  arrows(fe[i]-stderr[i],i,fe[i]+stderr[i],i,code=3, angle=90)
}



finalLangs$RatedSpikiness.bin = finalLangs$RatedSpikiness >4

mcontrol = glmerControl(optCtrl = list(maxfun = 500000))

mb0 = glmer(RatedSpikiness.bin ~ 1 + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb1 = glmer(RatedSpikiness.bin ~ Cond + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb2 = glmer(RatedSpikiness.bin ~ Cond + Gen + (1 |Chain) + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb3 = glmer(RatedSpikiness.bin ~ Cond + Gen + Shape + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb4 = glmer(RatedSpikiness.bin ~ Cond + (Gen * Shape) + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb5 = glmer(RatedSpikiness.bin ~ (Cond*Gen) + (Gen * Shape) + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb6 = glmer(RatedSpikiness.bin ~ (Cond*Gen) + (Gen * Shape) + (Shape:Cond) + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)
mb7 = glmer(RatedSpikiness.bin ~ Cond * Gen * Shape + (1 |Chain)  + (1|Item), data=finalLangs, family=binomial, control = mcontrol)

anova(mb0,mb1,mb2,mb3,mb4,mb5,mb6,mb7)

fe = fixef(mb7)
stderr = summary(mb7)$coefficients[,2]
par(mar=c(4,17,2,2))
plot(1:length(fixef(mb7))~fixef(mb7), pch=16, xlim=c(-1,1),ylim=c(length(fe),1), xlab='Fixed effect', ylab='', yaxt='n')
axis(2,at=1:8, labels=names(fe), las=2)
abline(v=0)
for(i in 1:length(fe)){
  arrows(fe[i]-stderr[i],i,fe[i]+stderr[i],i,code=3, angle=90)
}


dotplot(ranef(mb7,  condVar=T))





m0 = lmer(increaseIconicity~ condition + (1+condition| chain) + (1+condition|gen), data=datax)
summary(m0)




