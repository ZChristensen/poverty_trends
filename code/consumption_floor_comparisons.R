#Assessing updates to PovcalNet
list.of.packages <- c("plyr","data.table","varhandle","ggplot2","reshape2","readr")
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


#Loading PovcalNet data
load("data/AGGPovcalScrapeSept2018.RData")

# agg_total=unique(agg_total)
# save(agg_total,file="data/AGGPovcalScrapeSept2018.RData")

smy_total=rbind(smy_total_high,smy_total_low)
smy_total=smy_total[which(smy_total$CoverageType %in% c("N","A")),]
setdiff(smy_total$CountryName,old_smy$CountryName)

#Consumption floor calculations
agg_total$ConsumptionFloor = agg_total$povertyLine*(1-(agg_total$p2/agg_total$pg))
agg_total$diff=abs(agg_total$hc-0.2)
regional.extpov = subset(agg_total, povertyLine==1.90)
GlobalExtPov = subset(regional.extpov, regionTitle=="World Total")
names(GlobalExtPov)[which(names(GlobalExtPov)=="ConsumptionFloor")] <- "Global.Consumption.Floor"
names(GlobalExtPov)[which(names(GlobalExtPov)=="hc")] <- "Global.Ext.HC"
keep=c("requestYear","Global.Consumption.Floor","Global.Ext.HC")
GlobalExtPov=GlobalExtPov[,keep,with=F]

regional.LMpov=subset(agg_total, povertyLine==3.2)
GlobalLMpov=subset(regional.LMpov,regionTitle=="World Total")
names(GlobalLMpov)[which(names(GlobalLMpov)=="hc")] = "Global.LM.HC"
GlobalLMpov=GlobalLMpov[,c("requestYear","Global.LM.HC"), with=F]

regional.UMpov=subset(agg_total, povertyLine==5.5)
GlobalUMpov=subset(regional.UMpov,regionTitle=="World Total")
names(GlobalUMpov)[which(names(GlobalUMpov)=="hc")] = "Global.UM.HC"
GlobalUMpov=GlobalUMpov[,c("requestYear","Global.UM.HC"),with=F]

GlobalThreeLines=join_all(list(GlobalExtPov,GlobalLMpov,GlobalUMpov),by=c("requestYear"))


regional.p20 = data.table(agg_total)[,.SD[which.min(diff)],by=.(regionTitle,requestYear)]
WorldP20threshold = subset(regional.p20, regionTitle=="World Total")
WorldP20threshold$P20Threshold = WorldP20threshold$povertyLine
WorldP20threshold$P20pop=WorldP20threshold$population*.2
WorldP20threshold$P20average=WorldP20threshold$povertyLine -((WorldP20threshold$povertyLine*(WorldP20threshold$pg)*WorldP20threshold$population)/(WorldP20threshold$population*.2))
WorldP20threshold$restpop = WorldP20threshold$pop - WorldP20threshold$P20pop
WorldP20threshold$Restaverage=((WorldP20threshold$mean/(365/12)*WorldP20threshold$pop)-(WorldP20threshold$P20average*WorldP20threshold$P20pop))/WorldP20threshold$restpop


World=WorldP20threshold[,c("requestYear","P20average","ConsumptionFloor","Restaverage")]
World=join(World,GlobalThreeLines,by=c("requestYear"))
World=World[order(World$requestYear),]
World[,c("P20_growth","rest_growth","ext.hc.growth","lm.hc.growth","um.hc.gowth","yrchange"):=
        list(c(NA,diff(.SD$P20average))
             ,c(NA,diff(.SD$Restaverage))
             ,c(NA,diff(.SD$Global.Ext.HC))
             ,c(NA,diff(.SD$Global.LM.HC))
             ,c(NA,diff(.SD$Global.UM.HC))
             ,c(NA,diff(.SD$requestYear)))]
World$P20_rate=(World$P20_growth/World$yrchange)/World$P20average
World$Rest_rate=(World$rest_growth/World$yrchange)/World$Restaverage
World$ext_hc_rate=(World$ext.hc.growth/World$yrchange)
World$lm_hc_rate=(World$lm.hc.growth/World$yrchange)
World$um_hc_rate=(World$um.hc.growth/World$yrchange)

p=ggplot(World, aes(x=requestYear))+
  geom_line(aes(x=requestYear,y=Rest_rate,color="Rest_rate"))+
  geom_line(aes(x=requestYear,y=P20_rate,color="P20_rate"))+
  labs(x="Year",y="Growth rate of average income")+
  scale_y_continuous(labels=scales::percent)+
  theme_classic()+
  theme(legend.title=element_blank())
ggsave("data/graphics/ave_growth_rt_P20_rest.jpg",p)
p=ggplot(WorldP20threshold, aes(x=requestYear))+
  geom_line(aes(x=requestYear,y=Restaverage,color="Restaverage"))+
  geom_line(aes(x=requestYear,y=P20average,color="P20average"))+
  scale_y_continuous(labels=scales::dollar)+
  labs(x="Year",y="Average daily income per capita\n$2011 PPP",title="The gap between the P20 and the rest of the population is growing")+
  theme_classic()+
  theme(legend.title=element_blank())
ggsave("data/graphics/p20_rest_gap_trends.jpg",p)

gapgraph=World[,c("requestYear","Restaverage","P20average")]
gapgraph=gapgraph[which(gapgraph$requestYear>1999)]
write.csv(gapgraph,"data/P20_Rest_income_trends.csv",row.names=F, na="")

p=ggplot(World[which(World$requestYear>=1999),], aes(x=requestYear))+
  geom_line(aes(x=requestYear,y=Global.Consumption.Floor))+
  labs(x="Year",y="Consumption per capita\n$2011 PPP",title="The global consumption floor is declining")+
  scale_y_continuous(labels=scales::dollar)+
  theme_classic()
ggsave("data/graphics/consumpton_floor_Ravallion_1999_2015.jpg",p)
consumption_floor=World[which(World$requestYear>=1999),]
consumption_floor=consumption_floor[,c("requestYear","Global.Consumption.Floor")]
write.csv(consumption_floor,"data/consumption_floor_1999_2015.csv",row.names=F,na="")

##Looking at modal consumption levels

agg_total=agg_total[order(agg_total$regionCID,agg_total$requestYear,agg_total$povertyLine),]
agg_total[,c("Hdiff"):=list(c(NA,diff(.SD$hc))),by=c("regionCID","requestYear")]
cfloor=data.table(agg_total[which(agg_total$povertyLine<10)])[,.SD[which.max(Hdiff)],by=c("regionCID","requestYear")]
cfloor$PLminus=cfloor$povertyLine-.01
cfloorminus1=cfloor[,c("PLminus","regionCID","requestYear")]
agg_total2=join(agg_total,cfloorminus1,by=c("regionCID","requestYear"))
cfloorminus=agg_total2[which(agg_total2$PLminus==agg_total2$povertyLine),]
cfloorminus$hcminus=cfloorminus$hc
cfloorminus=cfloorminus[,c("hcminus","regionCID","requestYear")]
cfloor=join(cfloor,cfloorminus,by=c("regionCID","requestYear"))
cfloor$densitymode=cfloor$hc-cfloor$hcminus

ggplot(cfloor, aes(x=requestYear,group=regionCID,color=regionCID))+
  geom_line(aes(x=requestYear,y=povertyLine))+
  labs(x="Year",y="Daily consumption per capita\n$2011 PPP",title="Consumption Floor\nbased on modal income")+
  theme_classic()

World2=World[,c("requestYear","Global.Consumption.Floor")]
cfloor=cfloor[,c("regionCID","requestYear","povertyLine")]
cfloor=cfloor[which(cfloor$regionCID=="WLD"),]
cfloor=join(cfloor,World2,by=c("requestYear"))

names(cfloor)=c("regionCode","Year","Modal.Consumption.Floor","Ravallion.Consumption.Floor")
floorcomparisons=ggplot(cfloor[which(cfloor$Year>=1999),], aes(x=requestYear,group=regionCode,color=regionCode))+
  geom_line(aes(x=Year,y=Ravallion.Consumption.Floor,color="Ravallion.Consumption.Floor"))+
  geom_line(aes(x=Year,y=Modal.Consumption.Floor,color="Modal.Consumption.Floor"))+
  labs(x="Year",y="Daily consumption per capita\n$2011 PPP",title="Comparison of Consumption Floor estimates")+
  scale_y_continuous(labels=scales::dollar)+
  theme_classic()+
  theme(legend.title=element_blank())

ggsave("data/graphics/comparisons_consumption_floors.jpg",floorcomparisons)

write.csv(cfloor,"data/modal_consumption_floor.csv",row.names=F, na="")