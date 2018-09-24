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

load("data/AGGPovcalScrape1May2018.RData")
load("data/SMYPovcalScrape1May2018_low.RData")
load("data/SMYPovcalScrape1May2018_high.RData")

agg_total=unique(agg_total)

# agg_total=unfactor(data.frame(agg_total))
# save(agg_total,file="data/AGGPovcalScrape1May2018.RData")
# smy_high=unfactor(data.frame(smy_high))
# smy_low=unfactor(data.frame(smy_low))
# save(smy_high,file="data/SMYPovcalScrape1May2018_high.RData")
# save(smy_low,file="data/SMYPovcalScrape1May2018_low.RData")

regional.extpov = subset(agg_total, PovertyLine==1.90)
regional.threetwenty=subset(agg_total, PovertyLine==3.20)
regional.fivefifty=subset(agg_total, PovertyLine==5.50)
                  
old_regional=rbind(regional.extpov,regional.threetwenty,regional.fivefifty)
old_regional$P2=as.numeric(old_regional$P2)
old_regional$PG=as.numeric(old_regional$PG)
old_regional$H=as.numeric(old_regional$H)
old_regional$PovertyLine=as.numeric(old_regional$PovertyLine)
old_regional$ConsumptionFloor = old_regional$PovertyLine*(1-(old_regional$P2/old_regional$PG))

smy_total=rbind(smy_high,smy_low)


smy_total=unfactor(data.frame(smy_total))
smy_total= subset(smy_total, displayMode==0|displayMode==2|displayMode==4|displayMode==5)
      smy.extpov=subset(smy_total,PovertyLine==1.90)
      smy.threetwenty=subset(smy_total,PovertyLine==3.2)
      smy.fivefifty=subset(smy_total,PovertyLine==5.5)

old_smy=rbind(smy.extpov,smy.threetwenty,smy.fivefifty)


#Loading PovcalNet data
load("data/AGGPovcalScrapeSept2018.RData")
load("data/SMYPovcalScrapeSept2018_low.RData")
load("data/SMYPovcalScrapeSept2018_high.RData")

# agg_total=unique(agg_total)
# save(agg_total,file="data/AGGPovcalScrapeSept2018.RData")

smy_total=rbind(smy_total_high,smy_total_low)
smy_total=smy_total[which(smy_total$CoverageType %in% c("N","A")),]
setdiff(smy_total$CountryName,old_smy$CountryName)
setdiff(old_smy$CountryName,smy_total$CountryName)
old_smy$CountryName[which(old_smy$CountryName=="Swaziland")]="Eswatini"


#Global Check
colnames(old_regional) =c("requestYear",
                     "regionTitle"
                     ,"regionCID"
                     ,"povertyLine"
                     ,"oldmean"
                     ,"oldhc"         
                    ,"oldpg"
                    ,"oldp2"
                    ,"oldpopulation"
                    ,"oldConsumptionFloor")


setdiff(old_regional$requestYear,agg_total$requestYear)
setdiff(agg_total$requestYear,old_regional$requestYear)


regions=join(agg_total,old_regional, by=c("requestYear","regionCID","povertyLine"))
agg190=agg_total[which(agg_total$povertyLine==1.9),]
agg190=agg190[order(agg190$regionCID),]
agg190=agg190[order(agg190$requestYear),]
agg190[,c("hc_growth","yrchange"):=list(c(NA,diff(.SD$hc)),c(NA,diff(.SD$requestYear))),by=.(regionCID)]
agg190$annualized_hc_growth=agg190$hc_growth/agg190$yrchange
ggplot(agg190, aes(x=requestYear,y=hc,group=regionCID,color=regionCID))+geom_line()+theme_classic()
ggplot(agg190[which(requestYear>=1999),], aes(x=requestYear,y=hc,group=regionCID,color=regionCID))+geom_line()+theme_classic()
ggplot(agg190[which(requestYear>=2010),], aes(x=requestYear,y=hc,group=regionCID,color=regionCID))+geom_line()+theme_classic()
ggplot(agg190[which(regionCID=="WLD"),], aes(x=requestYear,y=annualized_hc_growth,group=regionCID,color=regionCID))+geom_line()+theme_classic()
ggplot(agg190[which(requestYear>=1999),], aes(x=requestYear,y=annualized_hc_growth,group=regionCID,color=regionCID))+geom_line()+theme_classic()
ggplot(agg190[which(requestYear>=2010),], aes(x=requestYear,y=annualized_hc_growth,group=regionCID,color=regionCID))+geom_line()+theme_classic()



#Regional Check
regions$oldhc=regions$oldhc/100
regions$hcdiff=regions$oldhc-regions$hc
regions$hcdiff_pct=regions$hcdiff/regions$oldhc
big_diff_reg=subset(regions, abs(hcdiff_pct)>.02)
##Big revisions seen for 2005 EAP, decreased HC by 6.9 percentage points, dropping global estimate by 2%
eap=regions[which(regions$regionCID=="EAP"& regions$povertyLine==1.9),]
eap=eap[,c("requestYear","hc","oldhc")]
eap.m=melt(eap,id.vars="requestYear")
ggplot(eap.m,aes(x=requestYear,y=value,group=variable,color=variable))+geom_line()+theme_classic()

# ohi=regions[which(regions$regionCID=="OHI"& regions$povertyLine==1.9),]
# ohi=ohi[,c("requestYear","hc","oldhc")]
# ohi.m=melt(ohi,id.vars="requestYear")
# ggplot(ohi.m,aes(x=requestYear,y=value,group=variable,color=variable))+geom_line()



#Country Check
smy_2015=smy_total[which(smy_total$RequestYear==2015),]
smy_2013=smy_total[which(smy_total$RequestYear==2013),]
setnames(smy_2013,"HeadCount","HeadCount2013")

comparisonsmy=join(smy_2015,smy_2013,by=c("CountryName","PovertyLine"))
comparisonsmy$hcdiff=comparisonsmy$HeadCount-comparisonsmy$HeadCount2013
comparisonsmy$hc_growth=comparisonsmy$hcdiff/comparisonsmy$HeadCount2013
comparisonsmy190=comparisonsmy[which(comparisonsmy$PovertyLine==1.9),]
pov_inc=subset(comparisonsmy190, comparisonsmy190$hcdiff>0)
length(unique(smy_2015$CountryName))
length(unique(pov_inc$CountryName))
unique(comparisonsmy190$CountryName[which(comparisonsmy190$hcdiff>.01)])
unique(comparisonsmy190$CountryName[which(comparisonsmy190$hcdiff>0)])
toppoverty=comparisonsmy190$CountryName[which(rank(comparisonsmy190$hcdiff)<20)]
toppov=smy_total[which(smy_total$CountryName %in% toppoverty & smy_total$PovertyLine==1.9 & smy_total$RequestYear>2005)]
toppov=toppov[,c("RequestYear","HeadCount","CountryName")]

ggplot(toppov,aes(x=RequestYear,y=HeadCount,group=CountryName,color=CountryName))+geom_line()+theme_classic()

CLB=c("Benin",
      "Burundi"
      ,"Central African Republic"
      ,"Chad"
      ,"Congo, Republic of"
      ,"Congo, Democratic Republic of"
      ,"Gambia, The"
      ,"Guinea"
      ,"Guinea-Bissau"
      ,"Haiti"
      ,"Lesotho"
      ,"Liberia"
      ,"Madagascar"
      ,"Malawi"
      ,"Micronesia, Federated States of"
      ,"Mozambique"
      ,"Niger"
      ,"Nigeria"
      ,"Papua New Guinea"
      ,"South Sudan"
      ,"Syrian Arab Republic"
      ,"Togo"
      ,"Uganda"
      ,"Yemen, Republic of"
      ,"Zambia")
povclb=smy_total[which(smy_total$CountryName %in% CLB & smy_total$PovertyLine==1.9),]
ggplot(povclb[which(povclb$RequestYear>2012)], aes(x=RequestYear))+
  geom_line(aes(x=RequestYear,y=HeadCount,group=CountryName,color=CountryName))
smy_total$CLB=NA
smy_total$CLB[which(smy_total$CountryName %in% CLB)]=1
clb.tab=data.table(smy_total[which(smy_total$PovertyLine==1.9)])[,.(
  HC=mean(HeadCount)
),by=c("RequestYear","CLB")]

#Elephant graph

masterlist=list()
masterlist.index=1
for (i in
     c(seq(.01,.99,by=.01),
       seq(.991,1,by=.001)
     )){
  print(i)
  new=data.table(agg_total[which(agg_total$regionCID=="WLD")])[,.SD[which.min(abs(hc-i))],by=c("requestYear")]
  masterlist[[masterlist.index]]=new
  masterlist.index=masterlist.index+1
}
master=rbindlist(masterlist)
master=master[,c("hc","povertyLine","requestYear","regionCID")]

master=master[order(master$requestYear, master$hc),]
master$HC=c(seq(1,99,by=1),
            seq(99.1,100,by=.1))

years=c(unique(master$requestYear))
years=years[order(years)]
headcounts=unique(master$hc)
countries=unique(master$regionCID)

master=data.frame(master)

if(exists("masterwide")){
  rm(masterwide)
}
for(year in years){ 
    year.values=c()
    year.hcs=c()
    year.nam=paste0("Income",year)
    year.values=subset(master, requestYear==year)[c("HC","povertyLine")]
    setnames(year.values,"povertyLine",year.nam)
    if(exists("masterwide")){
      masterwide=merge(masterwide,year.values,by="HC",all=T)
    }
    else{
      masterwide=year.values
    }
  }


masterwide$growth87to08rt = ((masterwide$Income2008-masterwide$Income1987)/masterwide$Income1987)/(2008-1987)
masterwide$growth03to15rt = ((masterwide$Income2015-masterwide$Income2002)/masterwide$Income2002)/(2015-2002)
masterwide$growth93to15rt = ((masterwide$Income2015-masterwide$Income1993)/masterwide$Income1993)/(2015-1993)
masterwide$growth08to15rt = ((masterwide$Income2015-masterwide$Income2008)/masterwide$Income2008)/(2015-2008)
masterwide$growth13to15rt = ((masterwide$Income2015-masterwide$Income2013)/masterwide$Income2013)/(2015-2013)
ggplot(masterwide[which(masterwide$HC!=100),],aes(x=HC))+
  geom_line(aes(x=HC,y=growth13to15rt))+
  # geom_line(aes(x=HC,y=growth03to15rt))+
  # geom_line(aes(x=HC,y=growth93to15rt))+
  # geom_line(aes(x=HC,y=growth08to15rt))+
  labs(x="Global Income Percentile",y="Income growth rate\n2013-2015")+
  scale_y_continuous(labels= scales::percent)+
  theme_classic()
  


#Consumption floor calculations
agg_total$ConsumptionFloor = agg_total$povertyLine*(1-(agg_total$p2/agg_total$pg))
agg_total$diff=abs(agg_total$hc-0.2)
regional.extpov = subset(agg_total, povertyLine==1.90)
GlobalExtPov = subset(regional.extpov, regionTitle=="World Total")
names(GlobalExtPov)[which(names(GlobalExtPov)=="ConsumptionFloor")] <- "Global.Consumption.Floor"
names(GlobalExtPov)[which(names(GlobalExtPov)=="hc")] <- "Global.Ext.HC"
keep=c("requestYear","Global.Consumption.Floor","Global.Ext.HC")
GlobalExtPov=GlobalExtPov[,keep,with=F]
regional.p20 = data.table(agg_total)[,.SD[which.min(diff)],by=.(regionTitle,requestYear)]
WorldP20threshold = subset(regional.p20, regionTitle=="World Total")
WorldP20threshold$P20Threshold = WorldP20threshold$povertyLine
WorldP20threshold$P20pop=WorldP20threshold$population*.2
WorldP20threshold$P20average=WorldP20threshold$povertyLine -((WorldP20threshold$povertyLine*(WorldP20threshold$pg)*WorldP20threshold$population)/(WorldP20threshold$population*.2))
WorldP20threshold$restpop = WorldP20threshold$pop - WorldP20threshold$P20pop
WorldP20threshold$Restaverage=((WorldP20threshold$mean/(365/12)*WorldP20threshold$pop)-(WorldP20threshold$P20average*WorldP20threshold$P20pop))/WorldP20threshold$restpop


World=WorldP20threshold[,c("requestYear","P20average","ConsumptionFloor","Restaverage")]
World=join(World,GlobalExtPov,by=c("requestYear"))
World=World[order(World$requestYear),]
World[,c("P20_growth","rest_growth","yrchange"):=list(c(NA,diff(.SD$P20average)),c(NA,diff(.SD$Restaverage)),c(NA,diff(.SD$requestYear)))]
World$P20_rate=(World$P20_growth/World$yrchange)/World$P20average
World$Rest_rate=(World$rest_growth/World$yrchange)/World$Restaverage

ggplot(World, aes(x=requestYear))+
  geom_line(aes(x=requestYear,y=Rest_rate,color="Rest_rate"))+
  geom_line(aes(x=requestYear,y=P20_rate,color="P20_rate"))+
  labs(x="Year",y="Growth rate of average income")+
  theme_classic()
ggplot(WorldP20threshold[which(WorldP20threshold$requestYear==2013|WorldP20threshold$requestYear==2015),], aes(x=requestYear))+
  geom_line(aes(x=requestYear,y=Restaverage))+
  geom_line(aes(x=requestYear,y=P20average))+
  labs(x="Year",y="Average daily income per capita\n$2011 PPP",title="The gap between the P20 and the rest of the population is growing")+
  theme_classic()


ggplot(World[which(World$requestYear>=1999),], aes(x=requestYear))+
  geom_line(aes(x=requestYear,y=Global.Consumption.Floor))+
  labs(x="Year",y="Consumption per capita\n$2011 PPP",title="The global consumption floor is declining")+
  theme_classic()

##Looking at modal consumption levels

agg_total=agg_total[order(agg_total$regionCID,agg_total$requestYear,agg_total$povertyLine),]
agg_total[,c("Hdiff"):=list(c(NA,diff(.SD$hc))),by=c("regionCID","requestYear")]
cfloor=data.table(agg_total[which(agg_total$povertyLine<10)])[,.SD[which.max(Hdiff)],by=c("regionCID","requestYear")]

ggplot(cfloor, aes(x=requestYear,group=regionCID,color=regionCID))+
  geom_line(aes(x=requestYear,y=povertyLine))+
  labs(x="Year",y="Daily consumption per capita\n$2011 PPP",title="Consumption Floor\nlargest population at a given poverty line")+
  theme_classic()


