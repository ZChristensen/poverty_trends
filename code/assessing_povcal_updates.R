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

load("data/AGGPovcalScrapeSept2018.RData")
load("data/SMYPovcalScrapeSept2018_low.RData")
load("data/SMYPovcalScrapeSept2018_high.RData")

agg_total=unique(agg_total)
smy_total=rbind(smy_total_high,smy_total_low)

# agg_total=unfactor(data.frame(agg_total))
# save(agg_total,file="data/AGGPovcalScrape1May2018.RData")
# smy_high=unfactor(data.frame(smy_high))
# smy_low=unfactor(data.frame(smy_low))
# save(smy_high,file="data/SMYPovcalScrape1May2018_high.RData")
# save(smy_low,file="data/SMYPovcalScrape1May2018_low.RData")

regional.extpov = subset(agg_total, povertyLine==1.90)
regional.threetwenty=subset(agg_total, povertyLine==3.20)
regional.fivefifty=subset(agg_total, povertyLine==5.50)
                  
old_regional=rbind(regional.extpov,regional.threetwenty,regional.fivefifty)
old_regional$ConsumptionFloor = old_regional$povertyLine*(1-(old_regional$p2/old_regional$pg))




smy_total=unfactor(data.frame(smy_total))
smy_total=smy_total[which(smy_total$CoverageType %in% c("N","A")),]
smy.extpov=subset(smy_total,PovertyLine==1.90)
smy.threetwenty=subset(smy_total,PovertyLine==3.2)
smy.fivefifty=subset(smy_total,PovertyLine==5.5)

old_smy=rbind(smy.extpov,smy.threetwenty,smy.fivefifty)


#Loading PovcalNet data
load("data/AGGPovcalScrape3April2019.RData")
load("data/SMYPovcalScrape3April2019.RData")


agg_total=unique(agg_total)
smy_total=unique(smy_total)


smy_total=smy_total[which(smy_total$CoverageType %in% c("N","A")),]
setdiff(smy_total$CountryName,old_smy$CountryName)
setdiff(old_smy$CountryName,smy_total$CountryName)


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
                    ,"oldP2")


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
# smy_2013=smy_total[which(smy_total$RequestYear==2013),]
# setnames(smy_2013,"HeadCount","HeadCount2013")
oldsmy_2015=old_smy[which(old_smy$RequestYear==2015),]
setnames(oldsmy_2015,"HeadCount","OldHeadCount")
# comparisonsmy=join(smy_2015,smy_2013,by=c("CountryName","PovertyLine"))
comparisonsmy=join(smy_2015,oldsmy_2015,by=c("CountryName","PovertyLine"))
comparisonsmy$hcdiff=comparisonsmy$HeadCount-comparisonsmy$OldHeadCount
# comparisonsmy$hcdiff=comparisonsmy$HeadCount-comparisonsmy$HeadCount2013
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
