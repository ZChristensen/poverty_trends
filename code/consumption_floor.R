list.of.packages <- c("data.table","plyr","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

load("C:/Users/Zach/Documents/Poverty data/AGGPovcalScrape1May2018.RData")

agg_total=unfactor(data.frame(agg_total))
agg_total$P2 = as.numeric(agg_total$P2)
agg_total$PG = as.numeric(agg_total$PG)
agg_total$H = as.numeric(agg_total$H)
agg_total$ConsumptionFloor = agg_total$PovertyLine*(1-(agg_total$P2/agg_total$PG))

agg_total$diff10=abs(agg_total$H-10)
agg_total=subset(agg_total, RegionTitle=="World Total")
P20median=data.table(agg_total)[,.SD[which.min(diff10),],by=c("RegionTitle","RequestYear")]
P20median=P20median[,c("RequestYear","PovertyLine")]
names(P20median)=c("RequestYear","Median")

agg_total$diff20=abs(agg_total$H-20)
P20threshold=data.table(agg_total)[,.SD[which.min(diff20),],by=c("RegionTitle","RequestYear")]
P20threshold=P20threshold[,c("RequestYear","PovertyLine")]
names(P20threshold)=c("RequestYear","P20threshold")

agg_total=subset(agg_total,PovertyLine==1.9)
agg_total=agg_total[,c("RequestYear","ConsumptionFloor")]

dfs=list(P20threshold,P20median,agg_total)
final=join_all(dfs,by=c("RequestYear"))
final$ConsumptionFloor=round(final$ConsumptionFloor,2)
final$extreme.poverty.line=1.90
df2030=data.frame(RequestYear=2030,
                  P20threshold=NA,
                  Median=NA,
                  ConsumptionFloor=NA,
                  extreme.poverty.line=1.90,
                  Median.projection=1.90,
                  ConsumptionFloor.projection=1.90)
final=rbindlist(list(final,df2030),fill=T)

final$Median.projection[which(final$RequestYear==2013)]=final$Median[which(final$RequestYear==2013)]
final$ConsumptionFloor.projection[which(final$RequestYear==2013)]=final$ConsumptionFloor[which(final$RequestYear==2013)]

year.span=c(2014:2029)
interim.years=data.frame(RequestYear=year.span)
final=rbindlist(list(final,interim.years),fill=T)
final=final[order(final$RequestYear),]
final$extreme.poverty.line=1.90
final$Median.projection=na.approx(final$Median.projection,na.rm=F)
final$ConsumptionFloor.projection=na.approx(final$ConsumptionFloor.projection,na.rm=F)

write.csv(final,"C:/Users/Zach/Documents/ITEP 2018/Chapter 1 Poverty/ConsumptionFloor.csv",row.names = F,na="")
