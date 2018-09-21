#Assessing updates to PovcalNet
list.of.packages <- c("plyr","data.table","varhandle","ggplot2")
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

smy_total=rbind(smy_total_high,smy_total_low)
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


#Regional Check

regions$hcdiff=regions$oldhc-regions$hc

#Country Check
setdiff(unique(old_smy$CountryName),unique(smy_total$CountryName))
setdiff(unique(smy_total$CountryName),unique(old_smy$CountryName))

smy_2015=smy_total[which(smy_total$RequestYear==2015),]
smy_2013=smy_total[which(smy_total$RequestYear==2013),]
setnames(smy_2013,"H","H2013")
comparisonsmy=join(smy_2015,smy_2013,by=c("CountryName","PovertyLine"))
comparisonsmy$H2013=as.numeric(unfactor(comparisonsmy$H2013))
comparisonsmy$H=as.numeric(unfactor(comparisonsmy$H))
comparisonsmy$diff=comparisonsmy$H2013-comparisonsmy$H
comparisonsmy$diffpct=comparisonsmy$diff/comparisonsmy$H2013
pov_inc=subset(comparisonsmy, comparisonsmy$diff<=0)
length(unique(smy_2015$CountryName))
length(unique(pov_inc$CountryName))
pov_dec=subset(comparisonsmy, comparisonsmy$diff>0)
length(unique(pov_dec$CountryName))
