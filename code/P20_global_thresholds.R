#Assessing updates to PovcalNet
list.of.packages <- c("plyr","data.table","varhandle","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


#Loading PovcalNet data
if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}
wd = paste0(prefix,"/git/poverty_trends/")
setwd(wd)

P20trends=read.csv("data/P20incometrends.csv")
P20trends=P20trends[which(!is.na(P20trends$P20Threshold)),]
P20thresholds=data.table(P20trends)[,.(P20threshold=mean(P20Threshold)),by=c("RequestYear")]
write.csv(P20thresholds,"data/P20thresholds.csv",row.names=F,na="")
