###DOWNLOAD THE PRE-BUILT COUNTRY TABLE AND SAVE IT FIRST

list.of.packages <- c("Hmisc","foreign","data.table","plyr","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/poverty_trends")
setwd(wd)

povcalcuts <- read.csv("data/P20incometrends.csv",as.is=TRUE)

excelpovcal=read.csv("data/CountryTable_1.9.csv")
excelpovcal=excelpovcal[which(excelpovcal$CoverageType %in% c("A","N")),]
excelpovcal=excelpovcal[,c("PovGap","HeadCount","RequestYear","CountryName")]
names(excelpovcal)=c("PovGapExcel","HeadCountExcel","RequestYear","CountryName")
povcalcuts=join(povcalcuts,excelpovcal,by=c("RequestYear","CountryName"))
povcalcuts$diff=povcalcuts$ExtPovHC-povcalcuts$HeadCountExcel

diffs=povcalcuts[which(abs(povcalcuts$diff)>0),]
if(nrow(diffs)>0){
  stop("differences with extreme poverty headcounts")
}
povcalcuts$diff=povcalcuts$ExtPovGap-povcalcuts$PovGapExcel
diffs=povcalcuts[which(abs(povcalcuts$diff)>0),]
if(nrow(diffs)>0){
  stop("differences with extreme poverty gaps")
}


metadata <- read.csv("data/metadata.csv",as.is=TRUE,na.strings="")
metadata=metadata[,c("PovcalNet.name","di_id")]
metadata=subset(metadata, di_id!="#N/A" & PovcalNet.name!="#N/A")
names(metadata)[1]="CountryName"

data.list=list()
data.index=1
for(year in 1981:2017){
  yearset= list()
  for(country in c(unique(povcalcuts$CountryName))){
  yearset[["RequestYear"]]=year  
  yearset[["CountryName"]]=country
  yearsetlist=data.frame(yearset)
  data.list[[data.index]]=yearsetlist
  data.index=data.index+1
  }
}

povcalallyrs = data.frame(rbindlist(data.list,fill=T))

if(length(setdiff(povcalallyrs$CountryName,metadata$CountryName))>0){
  stop("check country names")
}

povcalcutslong <- join(povcalallyrs,povcalcuts,by=c("RequestYear","CountryName"),type="left")
povcalcutslong=subset(povcalcutslong,!is.na(CountryName))
povcalcutslong=povcalcutslong[order(povcalcutslong$CountryName,povcalcutslong$RequestYear),]
povcalcutslong=join(povcalcutslong,metadata,by="CountryName",type="left")
# povcalcutslong$pop=as.numeric(povcalcutslong$Populations)*1000000
povcalcutslong$ExtPovPop= round(povcalcutslong$ExtPovHC * povcalcutslong$pop)
povcalcutslong$P20pop=round(povcalcutslong$P20pop)
dat=data.table(povcalcutslong)[,.(
                                  Year=RequestYear
                                  ,ExtPovHC=ExtPovHC
                                  ,ExtPovHC.Interp=na.approx(ExtPovHC,rule=2)
                                  ,ExtPovHC.Interp.Trunc=na.approx(ExtPovHC,rule=2)
                                  ,ModeratePovertyPercentage=LMPovHC
                                  ,ModeratePovertyPercentage.Interp=na.approx(LMPovHC,rule=2)
                                  ,Poorpop=ExtPovPop,Poorpop.Interp=na.approx(ExtPovPop,rule=2)
                                  ,P20percentage=P20Headcount
                                  ,P20population=P20pop
                                  ,Depth.Of.Extreme.Poverty=ExtPovGap
                                  
                                  ),by=.(di_id)]


dat$problem=NA
dat$problem[which(dat$P20percentage>dat$ExtPovHC & dat$P20population<dat$Poorpop)]=1
probs=subset(dat, problem==1)
if(nrow(probs)>0){
  stop("P20 rate is higher than extreme poverty rate but P20 population is lower than poor population")
}
dat$problem=NA
dat$problem[which(dat$ExtPovHC!=dat$ExtPovHC.Interp|dat$ExtPovHC.Interp.Trunc!=dat$ExtPovHC|dat$ModeratePovertyPercentage!=dat$ModeratePovertyPercentage.Interp|dat$ExtPovHC>dat$ModeratePovertyPercentage|dat$Poorpop!=dat$Poorpop.Interp)]=1
probs=subset(dat, problem==1)
if(nrow(probs)>0){
  stop("something went wrong with interpolations")
}

dat$ExtPovHC.Interp.Trunc[which(dat$Year>2015)]=dat$ExtPovHC[which(dat$Year>2015)]



poor_pop =dat[,c("di_id","Year","ExtPovHC","ExtPovHC.Interp.Trunc","ExtPovHC.Interp"),with=F]
pop_in_poverty = dat[,c("di_id","Year","Poorpop","Poorpop.Interp","P20population"),with=F]
pov_gap = dat[,c("di_id","Year","Depth.Of.Extreme.Poverty"),with=F]
less.than.3.20=dat[,c("di_id","Year","ModeratePovertyPercentage"),with=F]
Number.of.people.in.the.P20=dat[,c("di_id","Year","P20population"),with=F]
Percent.in.the.P20=dat[,c("di_id","Year","P20percentage"),with=F]


write.csv(data.frame(poor_pop),"data/data_warehouse/poor_pop.csv",row.names=F, na="")
write.csv(data.frame(pop_in_poverty),"data/data_warehouse/pop_in_poverty.csv",row.names=F, na="")
write.csv(data.frame(pov_gap),"data/data_warehouse/depth_of_extreme_poverty.csv",row.names=F,na="")
write.csv(data.frame(less.than.3.20),"data/data_warehouse/percent_of_population_living_less_than_320_a_day.csv",row.names=F,na="")
write.csv(data.frame(Number.of.people.in.the.P20),"data/data_warehouse/number_of_people_in_P20.csv",row.names=F,na="")
write.csv(data.frame(Percent.in.the.P20),"data/data_warehouse/percent_in_P20.csv",row.names=F,na="")
