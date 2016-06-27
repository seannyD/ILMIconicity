library(gplots)
library(car)
library(lme4)
setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)

plotmeans(finalLangs$RatedSpikiness~finalLangs$Item, las=2, xlab=F)

summary(aov(RatedSpikiness ~ Cond + Gen + Shape , data=finalLangs))

m0 = lmer(RatedSpikiness ~ 1 + (1 |Chain) , data=finalLangs)
m1 = lmer(RatedSpikiness ~ Cond + (1 |Chain) , data=finalLangs)
m2 = lmer(RatedSpikiness ~ Cond + Gen + (1 |Chain) , data=finalLangs)
m3 = lmer(RatedSpikiness ~ Cond + Gen + Shape + (1 |Chain) , data=finalLangs)
m4 = lmer(RatedSpikiness ~ Cond + Gen * Shape + (1 |Chain) , data=finalLangs)
m5 = lmer(RatedSpikiness ~ (Cond*Gen) + (Gen * Shape) + (1 |Chain) , data=finalLangs)
m6 = lmer(RatedSpikiness ~ (Cond*Gen) + (Gen * Shape) + (Shape:Cond) + (1 |Chain) , data=finalLangs)
m7 = lmer(RatedSpikiness ~ Cond * Gen * Shape + (1 |Chain) , data=finalLangs)

anova(m0,m1,m2,m3,m4,m5,m6,m7)
