list.of.packages <- c("data.table","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


#Loading PovcalNet data
if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}
wd = paste0(prefix,"/git/poverty_trends")
setwd(wd)
source("code/povcal_api.R")

# agg_results = list()
# smy_results = list()
# list_index = 1
# 
# load("data/povcal_list_tmp.RData")
# 
# for(povline in c(
#   seq(from=0,to=10,by=0.01),
#   seq(from=10.25,to=25.5,by=0.25),
#   seq(from=25.525,to=35.5,by=0.025),
#   seq(from=36,to=500,by=1),
#   seq(from=505,to=800,by=5))
#   ){
#   message(povline)
#   agg = povcal_agg(pl=povline)
#   smy = povcal_smy(pl=povline)
#   agg_results[[list_index]] = agg
#   smy_results[[list_index]] = smy
#   list_index=list_index+1
# }
# 
# save(agg_results,smy_results,list_index,file="data/povcal_list_tmp.RData")
# 
# agg_total=rbindlist(agg_results)
# smy_total=rbindlist(smy_results)
# save(smy_total,file="E:/git/poverty_trends/data/SMYPovcalScrapeMarch2019.RData")
# save(agg_total,file="E:/git/poverty_trends/data/AGGPovcalScrapeMarch2019.RData")
# smy_total_low = subset(smy_total,PovertyLine<=10)
# smy_total_high = subset(smy_total,PovertyLine>10)
# save(smy_total_low,file="E:/git/poverty_trends/data/SMYPovcalScrapeMarch2019_low.RData")
# save(smy_total_high,file="E:/git/poverty_trends/data/SMYPovcalScrapeMarch2019_high.RData")

# load("C:/Users/Zach/Documents/Poverty data/SMYPovcalScrape1May2018.RData")
# load("C:/Users/Zach/Documents/Poverty data/AGGPovcalScrape1May2018.RData")
# wd="C:/Users/Zach/Documents/Poverty data"
# setwd(wd)

load("data/SMYPovcalScrape6April2019.RData")
load("data/AGGPovcalScrape6April2019.RData")


smy_total=subset(smy, CoverageType %in% c("N","A"))
smy_total=unique(smy_total)
agg_total=unique(agg)
#This code calculates p20 threshold
#https://github.com/akmiller01/alexm-util/blob/master/DevInit/datahub_auto/povcal_calc.py
rm(smy,agg)
gc()

agg_total$ConsumptionFloor = agg_total$povertyLine*(1-(agg_total$p2/agg_total$pg))
agg_total$diff=abs(agg_total$hc-0.2)
regional.extpov = subset(agg_total, povertyLine==1.90)
GlobalExtPov = subset(regional.extpov, regionTitle=="World Total")
names(GlobalExtPov)[which(names(GlobalExtPov)=="ConsumptionFloor")] <- "Global.Consumption.Floor"
names(GlobalExtPov)[which(names(GlobalExtPov)=="hc")] <- "Global.Ext.HC"
keep=c("requestYear","Global.Consumption.Floor","Global.Ext.HC")
GlobalExtPov=GlobalExtPov[,keep]
regional.p20 = data.table(agg_total)[,.SD[which.min(diff)],by=.(regionTitle,requestYear)]
WorldP20threshold = subset(regional.p20, regionTitle=="World Total")
WorldP20threshold$P20Threshold = WorldP20threshold$povertyLine
keep=c("requestYear","P20Threshold")
WorldP20threshold2=data.frame(WorldP20threshold)[,keep]

smy_total$ConsumptionFloor= smy_total$PovertyLine*(1-(smy_total$PovGapSqr/smy_total$PovGap))
smy_total$diff = abs(smy_total$HeadCount - 0.20)
smy_total=join(smy_total, WorldP20threshold2, by="RequestYear")
smy_P20 = subset(smy_total, PovertyLine==P20Threshold)
names(smy_P20)[which(names(smy_P20)=="HeadCount")] <- "P20Headcount"
keep=c("CountryName","RequestYear","PovertyLine","HeadCount","CountryCode","ConsumptionFloor")
smy_total2=smy_total[,keep]
countries.np20 = data.table(smy_total)[,.SD[which.min(diff)],by=.(CountryCode,RequestYear)]
names(countries.np20)[which(names(countries.np20)=="PovGap")] <- "NP20PG"
keep=c("CountryName","RequestYear","PovertyLine","HeadCount","CountryCode","NP20PG")
countries.np20=data.frame(countries.np20)[,keep]
colnames(countries.np20)[colnames(countries.np20)=="PovertyLine"] <-"NP20.Threshold"
smy_extremepov = subset(smy_total2,PovertyLine==1.90)
names(smy_extremepov)[which(names(smy_extremepov)=="HeadCount")] <- "ExtPovHC"
smy_extremepov$PovertyLine = NULL
smy_extremepov$ConsumptionFloor = NULL
smy_LMpov = subset(smy_total2,PovertyLine==3.20)
names(smy_LMpov)[which(names(smy_LMpov)=="HeadCount")] <- "LMPovHC"
smy_LMpov$PovertyLine = NULL
smy_LMpov$ConsumptionFloor = NULL
smy_UMpov = subset(smy_total2,PovertyLine==5.50)
names(smy_UMpov)[which(names(smy_UMpov)=="HeadCount")] <- "UMPovHC"
smy_UMpov$PovertyLine = NULL
smy_UMpov$ConsumptionFloor = NULL

dfs <- list(smy_P20,smy_extremepov,smy_LMpov,smy_UMpov,countries.np20)
P20main<- join_all(dfs,by=c("RequestYear","CountryCode","CountryName"))

rm("smy_P20","smy_extremepov","smy_LMpov","smy_UMpov","countries.np20")
gc()


#Calculate averages of P20 and rest
P20main= data.frame(P20main)
P20main=P20main[which(P20main$CoverageType %in% c("A","N")),]
P20main$pop = P20main$ReqYearPopulation * 1000000
P20main$P20pop = (P20main$P20Headcount)*(P20main$pop)
P20main$P20average = P20main$PovertyLine -((P20main$PovertyLine*(P20main$PovGap)*P20main$pop)/P20main$P20pop)
P20main$restpop = P20main$pop - P20main$P20pop
P20main$restaverage = (((P20main$Mean/(365/12))*P20main$pop)-(P20main$P20average * P20main$P20pop))/P20main$restpop


#Calculate averages of NP20 and rest
P20main$NP20pop = P20main$pop * .20
P20main$NP20average = P20main$NP20.Threshold -((P20main$NP20.Threshold*(P20main$NP20PG)*P20main$pop)/P20main$NP20pop)
P20main$restpop = P20main$pop - P20main$NP20pop
P20main$Nrestaverage = (((P20main$Mean/(365/12))*P20main$pop)-(P20main$NP20average * P20main$NP20pop))/P20main$restpop


setnames(GlobalExtPov,"requestYear","RequestYear")
P20main=join(P20main, GlobalExtPov, by="RequestYear")


write.csv(P20main,"data/P20incometrends.csv",row.names=FALSE,na="")
write.csv(P20main,"E:/git/income_trends/data/P20incometrends.csv",row.names=FALSE,na="")
