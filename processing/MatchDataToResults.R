setwd("~/Documents/MPI/MonicaIconicity/oldBackups/ILM_OB2_fromBlake/Outputs")


finalLangs = read.csv("../../../SelectionAnalysis/data/finalLanguages/FinalLanguages.csv", stringsAsFactors = F)

finalLangs = finalLangs[nchar(finalLangs$Word)>0,]
finalLangs = finalLangs[,apply(finalLangs,2,function(X){sum(is.na(X))})<10,]
finalLangs = finalLangs[finalLangs$Gen>0,]

#finalLangs = finalLangs[finalLangs$Cond=="Communication",]
finalLangs$Cond2 = paste(finalLangs$Cond,finalLangs$Chain,finalLangs$Gen)

folders = list.dirs()
#folders = folders[grepl("MT_",folders)]

flist= data.frame(folder=folders,date="", stringsAsFactors = F)
for(i in 2:nrow(flist)){
  dstring = substr(flist[i,]$folder,nchar(flist[i,]$folder)-13, nchar((flist[i,]$folder)))
  
  flist$date[i] = as.character(strptime(dstring,"%d-%m-%H-%M-%S"))
}

write.csv(flist[order(flist$date),], '../../../SelectionAnalysis/data/OrderOfRuns.csv', row.names = F)

flist = flist[order(flist$date),]

allData = data.frame(folder="",word="")

for(f in flist$folder){
  resFile = list.files(f, pattern='*.OUT')
  #resFile = resFile[grepl('com',resFile,ignore.case = T) | grepl('arn',resFile,ignore.case = T)]
  
  if(length(resFile)>0){
  d = read.delim(paste(f,"/", resFile,sep=''),sep='\t',quote="", stringsAsFactors = F, header=F)
  numNAs = apply(d,2,function(X){sum(is.na(X))})
  if(ncol(d)==35){
    d = d[,-10]
  }
  
  if(nrow(d)>34){
  names(d) = c("spk",'state','round','date','t1','t2','t3','context','xx','meanings','inputLang','x1','t3','x2','x3','x4','t4','x5','x6','x7','x7b','contextSize','x8','x9','x10','x11','x12','x13', 'word','x15','target','x16','t5','x17')
  
  if(d[1,]$contextSize!=6){
    d[1,] = c(d[1,1:18],"-",d[1,19:33])
  }
  }  
  
  if(nrow(d)>24){
    d = d[(nrow(d)-23):nrow(d),]
    dx = data.frame(folder=f,word=d$word)
    allData = rbind(allData,dx)
  }
  }
}
allData$folder= as.character(allData$folder)
allData$word= as.character(allData$word)

allData = allData[nchar(allData$folder)>0,]

conds = unique(finalLangs$Cond2)

finalLangs$ResultsFolder = NA
finalLangs$ResultsFolderAlt = NA
finalLangs$hits = NA

for(i in 1:length(conds)){
  condx = conds[i]
  fw = finalLangs[finalLangs$Cond2==condx,]$Word
  hits = tapply(allData$word,allData$folder, function(X){
    
    X1 = X[seq(1,length(X),2)]
    X2 = X[seq(2,length(X),2)]
    max(sum(X1 %in% fw),sum(X2 %in% fw),na.rm=T)
  })
 
  maxh = which(hits==max(hits,na.rm=T))
  maxh2 = maxh[1]
  if(length(maxh)>1){
    maxh2 = maxh[! names(maxh) %in% finalLangs$ResultsFolderAlt]
  }
  
  finalLangs[finalLangs$Cond2==condx,]$ResultsFolder = names(maxh)[1]
  finalLangs[finalLangs$Cond2==condx,]$ResultsFolderAlt = names(maxh2)[1]
  finalLangs[finalLangs$Cond2==condx,]$hits = max(hits,na.rm=T)
}




fl = tapply(finalLangs$ResultsFolder,finalLangs$Cond2, head,n=1)
flAlt = tapply(finalLangs$ResultsFolderAlt,finalLangs$Cond2, head,n=1)
flh = tapply(finalLangs$hits,finalLangs$Cond2, head,n=1)


write.csv(cbind(fl,flAlt,names(fl),flh), "../../../SelectionAnalysis/data/MatchTrialData.csv",row.names=F)



for(f in unique(finalLangs$ResultsFolder)){
  newf = paste("../../../SelectionAnalysis/data/trials2/",f,sep='')
  files = list.files(f)
  dir.create(newf)
  file.copy(paste(f,files,sep='/'), newf)
}

finalLangs = finalLangs[,-which(names(finalLangs)=='hits'),]

write.csv(finalLangs,"../../../SelectionAnalysis/data/finalLanguages/FinalLanguages_withResultsFolder.csv", row.names = F)
