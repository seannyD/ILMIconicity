setwd("~/Documents/MPI/MonicaIconicity/SelectionAnalysis/analysis/")

library(png)
library(XLConnect)

# finalLangs = read.csv("../data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)
# # convert labels to English
# finalLangs$Shape[finalLangs$Shape=="Picudo"] = "Spiky"
# finalLangs$Shape[finalLangs$Shape=="Redondo"] = "Round"
# 
# finalLangs$RatedSpikiness2 = finalLangs$RatedSpikiness
# finalLangs[finalLangs$Shape=="Round",]$RatedSpikiness2 = 5 - finalLangs[finalLangs$Shape=="Round",]$RatedSpikiness2
# 
# sort(tapply(finalLangs$RatedSpikiness2, paste(finalLangs$Cond,finalLangs$Chain,finalLangs$Gen),mean))



finalLangs <- readWorksheetFromFile("../data/finalLanguages/FinalLanguages.xlsx", sheet=1)

finalLangs$RatedSpikiness2 = finalLangs$RatedSpikiness
finalLangs[finalLangs$Shape=="Round",]$RatedSpikiness2 = 5 - finalLangs[finalLangs$Shape=="Round",]$RatedSpikiness2

finalLangs$filenames = paste(finalLangs$Item, ".png",sep='')

finalLangs$chaingen = paste(finalLangs$Cond,finalLangs$Chain,finalLangs$Gen)

sort(tapply(finalLangs[finalLangs$Gen==6,]$RatedSpikiness2, finalLangs[finalLangs$Gen==6,]$chaingen,mean))

sort(tapply(finalLangs$RatedSpikiness2[finalLangs$Gen==0], paste(finalLangs$Cond[finalLangs$Gen==0],finalLangs$Chain[finalLangs$Gen==0],finalLangs$Gen[finalLangs$Gen==0]),mean))


# make language string and figure out if the same input lang is used for two chains?
finalLangs = finalLangs[order(finalLangs$Item),]
max(sort(table(tapply(finalLangs$Word, finalLangs$chaingen, paste, collapse=' '))))


makeExampleGrid = function(dx, filename){
  
  order = c("RoundRed","RoundRedThick",'SpikyRed','SpikyRedThick',
            "RoundGreen","RoundGreenThick",'SpikyGreen','SpikyGreenThick',
            "RoundBlue","RoundBlueThick",'SpikyBlue','SpikyBlueThick')
  scalex = 0.5
  pdf(filename, width=12*scalex , height=8*scalex )
  par(mfrow=c(3,4), mar=c(1,1,2,1))
  
  for(i in 1:length(order)){
    
    labelx = paste(dx[dx$Item==order[i],]$Word,
                   " (",
                   round(dx[dx$Item==order[i],]$RatedSpikiness,2),
                   ")", sep='')
    
    
    plot(-1:1,-1:1,type='n', 
         xlab='',ylab='',
         bty='n', xaxt='n',yaxt='n',
         main=labelx)
    lim <- par()
    imagex = readPNG(paste("../data/images/",order[i],".png",sep=''))
    rasterImage(imagex, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  }
  dev.off()
}


dx = finalLangs[finalLangs$Cond=="Communication" & finalLangs$Chain==1 & finalLangs$Gen==0,]
makeExampleGrid(dx, '../results/graphs/examples/Gen0.pdf')

dx = finalLangs[finalLangs$Cond=="Communication" & finalLangs$Chain==1 & finalLangs$Gen==6,]
makeExampleGrid(dx, '../results/graphs/examples/Gen6_Communication.pdf')

dx = finalLangs[finalLangs$Cond=="Learn" & finalLangs$Chain==6 & finalLangs$Gen==6,]
makeExampleGrid(dx, '../results/graphs/examples/Gen6_Learn.pdf')




ratings = read.delim("../data/ratings/SpikinessRatings", sep='\t', stringsAsFactors = F)

max(ratings$Age)
min(ratings$Age)
table(ratings$Sex) / nrow(ratings)
