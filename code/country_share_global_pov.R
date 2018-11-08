#Assessing updates to PovcalNet
list.of.packages <- c("readr")
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

poorpopulations=read.csv("data/data_warehouse/pop_in_poverty.csv")
poorpopulations=subset(poorpopulations,Year==2015)

load("data/AGGPovcalScrapeSept2018.RData")
global=agg_total[which(agg_total$requestYear==2015 & agg_total$regionCID=="WLD"&agg_total$povertyLine==1.9),]
globalpoor=global$hc*global$population

poorpopulations$shareofpoor=poorpopulations$Poorpop/(globalpoor*1000000)

protraced_crises=c("SY"
                   ,"YE"
                   ,"PS"
                   ,"SS"
                   ,"SO"
                   ,"AF"
                   ,"SD"
                   ,"CD"
                   ,"CF"
                   ,"TD"
                   ,"HT"
                   ,"ML"
                   ,"NE"
                   ,"BF"
                   ,"MR"
                   ,"DJ"
)

protracted=poorpopulations[which(poorpopulations$di_id %in% protraced_crises),]
protracted=protracted[,c("di_id","Poorpop","shareofpoor")]
so=data.frame(di_id="SO",Poorpop="",shareofpoor="")
af=data.frame(di_id="AF",Poorpop="",shareofpoor="")
protracted=rbind(protracted,so,af)
write.csv(protracted,"data/share_of_poor_in_protracted_crises.csv",row.names=F,na="")