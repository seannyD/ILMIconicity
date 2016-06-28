library(gplots)
library(car)
library(lattice)
library(lme4)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)

finalLangs$RatedSpikiness = finalLangs$RatedSpikiness- mean(finalLangs$RatedSpikiness)

finalLangs$Cond = factor(finalLangs$Cond, levels=c("Learn","Communication"))
finalLangs$Shape = factor(finalLangs$Shape, levels=c("Redondo","Picudo"))

par(mar=c(8,4,2,2))
plotmeans(finalLangs$RatedSpikiness~finalLangs$Item, las=2, xlab=F)

summary(aov(RatedSpikiness ~ Cond * Gen * Shape , data=finalLangs))

summary(aov(RatedSpikiness ~ Gen * Shape , data=finalLangs[finalLangs$Cond=="Learn",]))

m0 = lmer(RatedSpikiness ~ 1 + (1 |Chain) + (1|Gen) , data=finalLangs)
m0B = lmer(RatedSpikiness ~ 1 + (1 |Chain) + (1|Gen) + (1|Item) , data=finalLangs)

anova(m0,m0B)

m0 = lmer(RatedSpikiness ~ 1 + (1 |Chain)  + (1|Item), data=finalLangs)
m1 = lmer(RatedSpikiness ~ Cond + (1 |Chain)  + (1|Item), data=finalLangs)
m2 = lmer(RatedSpikiness ~ Cond + Gen + (1 |Chain) + (1|Item), data=finalLangs)
m3 = lmer(RatedSpikiness ~ Cond + Gen + Shape + (1 |Chain)  + (1|Item), data=finalLangs)
m4 = lmer(RatedSpikiness ~ Cond + (Gen * Shape) + (1 |Chain)  + (1|Item), data=finalLangs)
m5 = lmer(RatedSpikiness ~ (Cond*Gen) + (Gen * Shape) + (1 |Chain)  + (1|Item), data=finalLangs)
m6 = lmer(RatedSpikiness ~ (Cond*Gen) + (Gen * Shape) + (Shape:Cond) + (1 |Chain)  + (1|Item), data=finalLangs)
m7 = lmer(RatedSpikiness ~ Cond * Gen * Shape + (1 |Chain)  + (1|Item), data=finalLangs)

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





m0 = lmer(increaseIconicity~ condition + (1+condition| chain) + (1+condition|gen), data=datax)
summary(m0)
