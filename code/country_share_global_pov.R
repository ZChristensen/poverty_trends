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
# af2015poorpop=(33736.49*.33)*1000
# so2015poorpop=(13908.13*.717)*1000
# so=data.frame(di_id="SO",Year="2015",Poorpop=so2015poorpop,Poorpop.Interp="",P20population="")
# af=data.frame(di_id="AF",Year="2015",Poorpop=af2015poorpop,Poorpop.Interp="",P20population="")
# poorpopulations=rbind(poorpopulations,so,af)

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



forecasts=read.csv("data/WEOpoverty_forecasts.csv")
forecasts$di_id=NA
forecasts$di_id[which(forecasts$CountryName=="Syrian Arab Republic")]="SY"
forecasts$di_id[which(forecasts$CountryName=="Yemen, Republic of")]="YE"
forecasts$di_id[which(forecasts$CountryName=="West Bank and Gaza")]="PS"
forecasts$di_id[which(forecasts$CountryName=="South Sudan")]="SS"
forecasts$di_id[which(forecasts$CountryName=="Sudan")]="SD"
forecasts$di_id[which(forecasts$CountryName=="Congo, Democratic Republic of")]="CD"
forecasts$di_id[which(forecasts$CountryName=="Central African Republic")]="CF"
forecasts$di_id[which(forecasts$CountryName=="Chad")]="TD"
forecasts$di_id[which(forecasts$CountryName=="Haiti")]="HT"
forecasts$di_id[which(forecasts$CountryName=="Mali")]="ML"
forecasts$di_id[which(forecasts$CountryName=="Niger")]="NE"
forecasts$di_id[which(forecasts$CountryName=="Burkina Faso")]="BF"
forecasts$di_id[which(forecasts$CountryName=="Mauritania")]="MR"
forecasts$di_id[which(forecasts$CountryName=="Djibouti")]="DJ"
# forecasts$di_id[which(forecasts$CountryName=="Somalia")]="SO"
# forecasts$di_id[which(forecasts$CountryName=="Afghanistan")]="AF"
forecasts$protracted=1
forecasts$protracted[which(is.na(forecasts$di_id))]=0
poorpop2030=data.table(forecasts)[,.(
  poorpops2030=sum(basepoorpop,na.rm=T)
),by=c("protracted")]

poorpop2030=data.table(forecasts)[,.(
  totalpoorpops2030=sum(basepoorpop,na.rm=T)
)]
forecasts$global_forecast_poorpops=poorpop2030$totalpoorpops2030
forecasts$share_forecasted_pov=forecasts$basepoorpop/forecasts$global_forecast_poorpops
forecasts=forecasts[,c("CountryName","di_id","share_forecasted_pov","basepoorpop")]
protracted=join(protracted,forecasts,by=c("di_id"))
protracted$poorpop2030=protracted$basepoorpop*1000000
protracted=protracted[,c("CountryName","di_id","Poorpop","shareofpoor","share_forecasted_pov","poorpop2030")]
write.csv(protracted,"data/share_of_poor_in_protracted_crises.csv",row.names=F,na="")
