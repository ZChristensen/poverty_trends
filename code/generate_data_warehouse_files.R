list.of.packages <- c("Hmisc","foreign","data.table","plyr","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

povcalcuts <- read.csv("C:/Users/Zach/Documents/Poverty data/P20incometrends20180505.csv",as.is=TRUE)
metadata <- read.csv("C:/Users/Zach/Documents/Poverty data/Country metadata.csv",as.is=TRUE,na.strings="")
keep=c("PovcalNet.name","di_id")
metadata=metadata[,keep]
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
wd<-"C:/Users/Zach/Documents/Poverty data/Data Hub/Povcal Updates April 2018"
setwd(wd)

povcalcutslong <- join(povcalallyrs,povcalcuts,by=c("RequestYear","CountryName"),type="left")
povcalcutslong=subset(povcalcutslong,!is.na(CountryName))
povcalcutslong=povcalcutslong[order(povcalcutslong$CountryName,povcalcutslong$RequestYear),]
povcalcutslong=join(povcalcutslong,metadata,by="CountryName",type="left")
povcalcutslong$ExtPovHC.pct = povcalcutslong$ExtPovHC/100
# povcalcutslong$pop=as.numeric(povcalcutslong$Populations)*1000000
povcalcutslong$ExtPovPop= povcalcutslong$ExtPovHC.pct * povcalcutslong$pop
dat=data.table(povcalcutslong)[,.(Year=RequestYear,ExtPovHC=ExtPovHC.pct,ExtPovHC.Interp=na.approx(ExtPovHC.pct,rule=2),ExtPovHC.Interp.Trunc=na.approx(ExtPovHC.pct,rule=2),ModeratePovertyPercentage=(LMPovHC/100),ModeratePovertyPercentage.Interp=na.approx((LMPovHC/100),rule=2),Poorpop=ExtPovPop,Poorpop.Interp=na.approx(ExtPovPop,rule=2),P20percentage=(P20Headcount/100),P20population=P20pop,Depth.Of.Extreme.Poverty=(PG.1/100)),by=.(di_id)]
dat=as.data.frame(dat)

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

dat$ExtPovHC.Interp.Trunc[which(dat$Year>2013)]=NA
keep=c("di_id","Year","ExtPovHC","ExtPovHC.Interp.Trunc","ExtPovHC.Interp")
poor_pop =dat[,keep]
keep=c("di_id","Year","Poorpop","Poorpop.Interp","P20population")
pop_in_poverty = dat[,keep]
keep=c("di_id","Year","Depth.Of.Extreme.Poverty")
pov_gap = dat[,keep]
keep=c("di_id","Year","ModeratePovertyPercentage")
less.than.3.10=dat[,keep]
keep=c("di_id","Year","P20population")
Number.of.people.in.the.P20=dat[,keep]
keep=c("di_id","Year","P20percentage")
Percent.in.the.P20=dat[,keep]


write.csv(data.frame(poor_pop),"poor_pop.csv",row.names=F, na="")
write.csv(data.frame(pop_in_poverty),"pop_in_poverty.csv",row.names=F, na="")
write.csv(data.frame(pov_gap),"depth_of_extreme_poverty.csv",row.names=F,na="")
write.csv(data.frame(less.than.3.10),"percent_of_population_living_less_than_310_a_dat.csv",row.names=F,na="")
write.csv(data.frame(Number.of.people.in.the.P20),"number_of_people_in_P20.csv",row.names=F,na="")
write.csv(data.frame(Percent.in.the.P20),"percent_in_P20.csv",row.names=F,na="")
