list.of.packages <- c("Hmisc","foreign","data.table","plyr","varhandle","WDI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/poverty_trends/")
setwd(wd)



load("data/AGGPovcalScrapeSept2018.RData")
load("data/SMYPovcalScrapeSept2018_low.RData")
load("data/SMYPovcalScrapeSept2018_high.RData")

agg_total=unique(agg_total)


regional.extpov = subset(agg_total, povertyLine==1.90)

old_regional=agg_total


old_regional$oldConsumptionFloor = old_regional$povertyLine*(1-(old_regional$p2/old_regional$pg))
old.agg.extpov=subset(old_regional,povertyLine==1.90 & regionTitle=="World Total")
old.consumption.floor=old.agg.extpov[,c("requestYear","oldConsumptionFloor")]
old.consumption.floor$oldConsumptionFloor=round(old.consumption.floor$oldConsumptionFloor,digits=2)

smy_total=rbind(smy_total_high,smy_total_low)


smy_total=unfactor(data.frame(smy_total))
setnames(old.consumption.floor,"requestYear","RequestYear")
old_smy= subset(smy_total, CoverageType=="N"|CoverageType=="A")
old_smy=join(old_smy,old.consumption.floor, by="RequestYear")
old_smy_consumption_floor=subset(old_smy, PovertyLine==oldConsumptionFloor)

load("data/AGGPovcalScrape6April2019.RData")
load("data/SMYPovcalScrape6April2019.RData")


agg=unique(agg)
smy_total=smy
smy_total=smy_total[which(smy_total$CoverageType %in% c("N","A")),]


agg$newConsumptionFloor = agg$povertyLine*(1-(agg$p2/agg$pg))
new.agg.extpov=subset(agg,povertyLine==1.90 & regionTitle=="World Total")
new.consumption.floor=new.agg.extpov[,c("requestYear","newConsumptionFloor")]
new.consumption.floor$newConsumptionFloor=round(new.consumption.floor$newConsumptionFloor,digits=2)
names(new.consumption.floor)=c("RequestYear","newConsumptionFloor")

smy_total=join(smy_total, new.consumption.floor, by="RequestYear")
new_smy_consumption_floor=subset(smy_total, PovertyLine==newConsumptionFloor)
new_smy_consumption_floor=new_smy_consumption_floor[,c("CountryCode","RequestYear","DataYear","HeadCount","PovGap","PovGapSqr")]
names(new_smy_consumption_floor)=c("CountryCode","RequestYear","newDataYear","newHeadCount","newPovGap","newPovGapSqr")
new_smy_consumption_floor$newHeadCount=new_smy_consumption_floor$newHeadCount * 100

smy_comparisons=join(old_smy_consumption_floor,new_smy_consumption_floor,by=c("CountryCode","RequestYear"))
smy_comparisons$Hdiff=smy_comparisons$HeadCount-smy_comparisons$newHeadCount
diffs=smy_comparisons[which(smy_comparisons$Hdiff>0),]

smy_total=unique(smy_total)
#Compare SMYs at 1.90
old_smy_ext=old_smy[which(old_smy$PovertyLine==1.9),]
old_smy_ext=old_smy_ext[,c("CountryCode","CountryName","RequestYear","HeadCount","PovGap","PovGapSqr","DataYear","oldConsumptionFloor")]
names(old_smy_ext)=c("CountryCode","CountryName","RequestYear","H","PG","P2","DataYear","oldConsumptionFloor")
new_smy_ext=smy_total[which(smy_total$PovertyLine==1.9),]
new_smy_ext=new_smy_ext[,c("CountryCode","RequestYear","HeadCount","PovGap","PovGapSqr","newConsumptionFloor")]
names(new_smy_ext)=c("CountryCode","RequestYear","oldHeadCount","oldPG","oldP2","newConsumptionFloor")
comparisons190=join(new_smy_ext,old_smy_ext, by=c("CountryCode","RequestYear"))
comparisons190$hcdiff=comparisons190$oldHeadcount-comparisons190$H
comparisons190$pgdiff=comparisons190$oldPG-comparisons190$PovGap
comparisons190$p2diff=comparisons190$oldP2-comparisons190$PovGapSqr
diff190s=comparisons190[which(abs(comparisons190$hcdiff)>.01| abs(comparisons190$pgdiff)>.01|abs(comparisons190$p2diff)>.01),]

pops=WDI(country="all",indicator="SP.POP.TOTL",start=1981, end=2017,extra=TRUE)
pops$country[which(pops$country=="Venezuela, RB")]="Venezuela, Republica Bolivariana de"
pops$country[which(pops$country=="Micronesia, Fed. Sts.")]="Micronesia, Federated States of"
pops=pops[,c("country","year","SP.POP.TOTL")]
names(pops)=c("CountryName","RequestYear","Population")
diff190s=join(diff190s,pops,by=c("CountryName","RequestYear"))
diff190s$popshift=diff190s$hcdiff*diff190s$Population

#comparing headcounts

hc_at_floor=join(old_smy_consumption_floor,new_smy_consumption_floor,by=c("CountryCode","RequestYear"))
