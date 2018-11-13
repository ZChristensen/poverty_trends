list.of.packages <- c("WDI","data.table","Hmisc","plyr","zoo","varhandle","jsonlite","curl")
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

#WEO
# WEO=read.delim("http://www.imf.org/external/pubs/ft/weo/2018/02/weodata/WEOOct2018all.xls", na.strings="n/a")
# write.csv(WEO,"data/WEOOct2018.csv",row.names = F,na="")
WEO=read.csv("data/WEOOct2018.csv")
WEO=subset(WEO, WEO.Subject.Code=="NGDP_RPCH")
WEO=unfactor(WEO)
WEO$growth15to17=(WEO$X2015+WEO$X2016+WEO$X2017)/3
WEO$growth15to20=(WEO$X2015+WEO$X2016+WEO$X2017+WEO$X2018+WEO$X2019+WEO$X2020)/6
WEO$growthforecast=(WEO$X2015+WEO$X2016+WEO$X2017+WEO$X2018+WEO$X2019+WEO$X2020+WEO$X2021+WEO$X2022+WEO$X2023)/9
forecasts=WEO[,c("Country","growth15to20","growth15to17","growthforecast")]

WEO$Country[which(WEO$Country=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
WEO$Country[which(WEO$Country=="Republic of Congo")]="Congo, Republic of"
WEO$Country[which(WEO$Country=="Côte d'Ivoire")]="Cote d'Ivoire"
WEO$Country[which(WEO$Country=="Egypt")]="Egypt, Arab Republic of"
WEO$Country[which(WEO$Country=="The Gambia")]="Gambia, The"
WEO$Country[which(WEO$Country=="Islamic Republic of Iran")]="Iran, Islamic Republic of"
WEO$Country[which(WEO$Country=="Russia")]="Russian Federation"
WEO$Country[which(WEO$Country=="Syria")]="Syrian Arab Republic"
WEO$Country[which(WEO$Country=="Venezuela")]="Venezuela, Republica Bolivariana de"
WEO$Country[which(WEO$Country=="Yemen")]="Yemen, Republic of"
WEO$Country[which(WEO$Country=="Lao P.D.R.")]="Lao People's Democratic Republic"
WEO$Country[which(WEO$Country=="FYR Macedonia")]="Macedonia, former Yugoslav Republic of"
WEO$Country[which(WEO$Country=="Micronesia")]="Micronesia, Federated States of"
WEO$Country[which(WEO$Country=="São Tomé and Príncipe")]="Sao Tome and Principe"
WEO$Country[which(WEO$Country=="Korea")]="Korea, Republic of"
WEO$Country[which(WEO$Country=="Swaziland")]="Eswatini"

names(WEO)[which(names(WEO)=="Country")]<- "CountryName"
WEO=WEO[,c("CountryName","growth15to20","growth15to17","growthforecast")]


#Read in PovcalNet data
load("data/SMYPovcalScrapeSept2018_high.RData")
load("data/SMYPovcalScrapeSept2018_low.RData")

smy_total=rbind(smy_total_high,smy_total_low)


smy_total=smy_total[which(smy_total$RequestYear==2015),]
som=(1+((WEO$growthforecast[which(WEO$CountryName=="Somalia")])/100))^12
print(som)
afg=(1+((WEO$growthforecast[which(WEO$CountryName=="Afghanistan")])/100))^12
print(afg)
merge.dat=join(smy_total, WEO, by="CountryName")

merge.dat$pl2020 = merge.dat$PovertyLine*(1+(merge.dat$growth15to20/100))^5
merge.dat$pl2018 = merge.dat$PovertyLine*(1+(merge.dat$growth15to17/100))^3
merge.dat$pl2030.base = merge.dat$pl2018*(1+(merge.dat$growthforecast/100))^12
merge.dat$pl2030.plusone = merge.dat$pl2018*(1+((merge.dat$growthforecast+1)/100))^12
merge.dat$pl2030.minusone = merge.dat$pl2018*(1+((merge.dat$growthforecast-1)/100))^12

merge.dat$basediff=abs(merge.dat$pl2030.base-1.9)
merge.dat$plusonediff=abs(merge.dat$pl2030.plusone-1.9)
merge.dat$minusonediff=abs(merge.dat$pl2030.minusone-1.9)
merge.dat$base2020=abs(merge.dat$base2020-1.9)
merge.dat=merge.dat[,c("CountryName"
                       ,"base2020"
                       ,"HeadCount"
                       ,"basediff"
                       ,"plusonediff"
                       ,"minusonediff"
                       ,"RegionCode")]


base=data.table(merge.dat)[,.SD[which.min(basediff)],by=.(CountryName)]
names(base)[which(names(base)=="HeadCount")]="BaseHC2030"
base=base[,c("CountryName","BaseHC2030","RegionCode")]
somalia=data.frame(CountryName="Somalia",BaseHC2030=".440",RegionCode="SSA")
afghanistan=data.frame(CountryName="Afghanistan",BaseHC2030=".144",RegionCode="SAS")
base=rbind(base,somalia,afghanistan)
base=as.data.frame(base)
plusone=data.table(merge.dat)[,.SD[which.min(plusonediff)],by=.(CountryName)]
names(plusone)[which(names(plusone)=="HeadCount")]="PlusOneHC2030"
plusone=plusone[,c("CountryName","PlusOneHC2030")]
plusone=as.data.frame(plusone)
minusone=data.table(merge.dat)[,.SD[which.min(minusonediff)],by=.(CountryName)]
names(minusone)[which(names(minusone)=="HeadCount")]="MinusOneHC2030"
minusone=minusone[,c("CountryName","MinusOneHC2030")]
minusone=as.data.frame(minusone)
base2020=data.table(merge.dat)[,.SD[which.min(basediff)],by=.(CountryName)]
names(base2020)[which(names(base2020)=="HeadCount")]="BaseHC2020"
base2020=base2020[,c("CountryName","BaseHC2020","RegionCode")]
dfs=list(base,plusone,minusone,base2020)
forecasts=join_all(dfs)
forecasts2=forecasts[which(forecasts$CountryName!="Somalia"),]
forecasts2=forecasts2[which(forecasts2$CountryName!="Afghanistan"),]
regionalforecasts=data.table(forecasts2)[,.(r.base=mean(as.numeric(BaseHC2030))
                                           ,r.minusone=mean(as.numeric(MinusOneHC2030))
                                           ,r.plusone=mean(as.numeric(PlusOneHC2030))
                                        ),by=.(RegionCode)]

#WorldPopulationProspects
# pop=read.csv("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv")
# 
# 
# names(pop)[which(names(pop)=="Location")]="CountryName"
# 
# pop$CountryName=unfactor(pop$CountryName)
# pop$CountryName[which(pop$CountryName=="Bolivia (Plurinational State of)")]="Bolivia"
# pop$CountryName[which(pop$CountryName=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
# pop$CountryName[which(pop$CountryName=="Congo")]="Congo, Republic of"
# pop$CountryName[which(pop$CountryName=="CÃ´te d'Ivoire")]="Cote d'Ivoire"
# pop$CountryName[which(pop$CountryName=="Czechia")]="Czech Republic"
# pop$CountryName[which(pop$CountryName=="Egypt")]="Egypt, Arab Republic of"
# pop$CountryName[which(pop$CountryName=="Gambia")]="Gambia, The"
# pop$CountryName[which(pop$CountryName=="Iran (Islamic Republic of)")]="Iran, Islamic Republic of"
# pop$CountryName[which(pop$CountryName=="Republic of Korea")]="Korea, Republic of"
# pop$CountryName[which(pop$CountryName=="Kyrgyzstan")]="Kyrgyz Republic"
# pop$CountryName[which(pop$CountryName=="TFYR Macedonia")]="Macedonia, former Yugoslav Republic of"
# pop$CountryName[which(pop$CountryName=="Micronesia (Fed. States of)")]="Micronesia, Federated States of"
# pop$CountryName[which(pop$CountryName=="Republic of Moldova")]="Moldova"
# pop$CountryName[which(pop$CountryName=="Slovakia")]="Slovak Republic"
# pop$CountryName[which(pop$CountryName=="Saint Lucia")]="St. Lucia"
# pop$CountryName[which(pop$CountryName=="United Republic of Tanzania")]="Tanzania"
# pop$CountryName[which(pop$CountryName=="United States of America")]="United States"
# pop$CountryName[which(pop$CountryName=="Venezuela (Bolivarian Republic of)")]="Venezuela, Republica Bolivariana de"
# pop$CountryName[which(pop$CountryName=="Viet Nam")]="Vietnam"
# pop$CountryName[which(pop$CountryName=="Yemen")]="Yemen, Republic of"
# pop$CountryName[which(pop$CountryName=="State of Palestine")]="West Bank and Gaza"
# 
# noncountries<- c("Africa"
#                  ,"Asia"
#                  ,"American Samoa"
#                  ,"Anguilla"
#                  ,"Australia/New Zealand"
#                  ,"British Virgin Islands"
#                  ,"Americaan Samoa"
#                  ,"Caribbean"
#                  ,"Caribbean Netherlands"
#                  ,"Cayman Islands"
#                  ,"Central America"
#                  ,"Central Asia"
#                  ,"Channel Islands"
#                  ,"China, Hong Kong SAR"                                       
#                  ,"China, Macao SAR"
#                  ,"CuraÃ§ao" 
#                  ,"Eastern Africa"                                             
#                  ,"Eastern Asia"                                               
#                  ,"Eastern Europe" 
#                  ,"Europe"
#                  ,"Faeroe Islands"
#                  ,"Falkland Islands (Malvinas)"
#                  ,"French Guiana"                                              
#                  ,"French Polynesia"                                           
#                  ,"Gibraltar"                                                  
#                  ,"Greenland"                                                  
#                  ,"Grenada"                                                    
#                  ,"Guam" 
#                  ,"Guadeloupe"
#                  ,"High-income countries"                                      
#                  ,"Isle of Man"                                                
#                  ,"Latin America and the Caribbean"                            
#                  ,"Least developed countries"                                  
#                  ,"Less developed regions"                                     
#                  ,"Less developed regions, excluding China"                    
#                  ,"Less developed regions, excluding least developed countries"
#                  ,"Low-income countries"                                       
#                  ,"Lower-middle-income countries"                              
#                  ,"Martinique"                                                 
#                  ,"Mayotte"                                                    
#                  ,"Melanesia"                                                  
#                  ,"Middle Africa"                                              
#                  ,"Middle-income countries"                                    
#                  ,"Montserrat"                                                 
#                  ,"More developed regions"                                     
#                  ,"New Caledonia"                                              
#                  ,"Northern Africa"                                            
#                  ,"Northern America"                                           
#                  ,"Northern Europe"                                            
#                  ,"Northern Mariana Islands"                                   
#                  ,"Oceania"                                                    
#                  ,"Polynesia"                                                  
#                  ,"Puerto Rico"                                                
#                  ,"RÃ©union"                                                   
#                  ,"Saint Helena"                                               
#                  ,"Saint Kitts and Nevis"                                      
#                  ,"Saint Vincent and the Grenadines"                           
#                  ,"Saint Pierre and Miquelon" 
#                  ,"Sint Maarten (Dutch part)"                                  
#                  ,"South America"                                              
#                  ,"South-Central Asia"                                         
#                  ,"South-Eastern Asia"                                         
#                  ,"Southern Africa"                                            
#                  ,"Southern Asia"                                              
#                  ,"Southern Europe"                                            
#                  ,"Sub-Saharan Africa"                                         
#                  ,"Turks and Caicos Islands" 
#                  ,"United States Virgin Islands"                               
#                  ,"Upper-middle-income countries" 
#                  ,"Wallis and Futuna Islands"
#                  ,"Western Africa"                                             
#                  ,"Western Asia"                                               
#                  ,"Western Europe"                                             
#                  ,"Western Sahara"                                             
#                  ,"World"                                                      
# )
# countries=setdiff(unique(pop$CountryName),noncountries)
# pop=subset(pop, CountryName %in% countries)
# write.csv(pop,"data/pops.csv",row.names=F,na="")
pop=read.csv("data/pops.csv")

pop2030=subset(pop, Time==2030 & Variant=="Medium")
pop2020=subset(pop, Time==2020 & Variant=="Medium")
names(pop2030)[which(names(pop2030)=="PopTotal")]="PopTotal2030"
names(pop2020)[which(names(pop2020)=="PopTotal")]="PopTotal2020"
pop2030=pop2030[,c("CountryName","PopTotal2030")]
pop2020=pop2020[,c("CountryName","PopTotal2020")]



forecasts=join(pop2030,forecasts,by="CountryName")
forecasts=join(pop2020,forecasts,by="CountryName")
forecasts$RegionCode[which(forecasts$CountryName=="Afghanistan")]="SAS"
forecasts$RegionCode[which(forecasts$CountryName=="Libya")]="MNA"
forecasts$RegionCode[which(forecasts$CountryName=="Syrian Arab Republic")]="MNA"
forecasts$RegionCode[which(forecasts$CountryName=="State of Palestine")]="MNA"
forecasts$RegionCode[which(forecasts$CountryName=="West Bank and Gaza")]="MNA"
forecasts$RegionCode[which(forecasts$CountryName=="Equatorial Guinea")]="SSA"
forecasts$RegionCode[which(forecasts$CountryName=="Eritrea")]="SSA"
forecasts$RegionCode[which(forecasts$CountryName=="Somalia")]="SSA"
forecasts$RegionCode[which(forecasts$CountryName=="Argentina")]="LAC"
forecasts$RegionCode[which(forecasts$CountryName=="Cuba")]="LAC"
forecasts$RegionCode[which(forecasts$CountryName=="Cambodia")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Marshall Islands")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Micronesia")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Cook Islands")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Dem. People's Republic of Korea")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Nauru")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Niue")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Palau")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Tokelau")]="EAP"
forecasts$RegionCode[which(forecasts$CountryName=="Andorra" )]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Antigua and Barbuda")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Aruba")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Bahamas")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Bahrain")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Barbados")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Bermuda")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Brunei Darussalam" )]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="China, Taiwan Province of China")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Dominica" )]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Guadeloupe")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Holy See")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Kuwait")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Liechtenstein")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Monaco")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="New Zealand")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Oman")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Qatar" )]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="San Marino")]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Saudi Arabia" )]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="Singapore" )]="OHI"
forecasts$RegionCode[which(forecasts$CountryName=="United Arab Emirates")]="OHI"


forecasts=join(forecasts,regionalforecasts,by=c("RegionCode"))
# forecasts$BaseHC2030[which(is.na(forecasts$BaseHC2030))]=forecasts$r.base[which(is.na(forecasts$BaseHC2030))]
# forecasts$PlusOneHC2030[which(is.na(forecasts$PlusOneHC2030))]=forecasts$r.plusone[which(is.na(forecasts$PlusOneHC2030))]
# forecasts$MinusOneHC2030[which(is.na(forecasts$MinusOneHC2030))]=forecasts$r.minusone[which(is.na(forecasts$MinusOneHC2030))]
forecasts$pop2030=forecasts$PopTotal2030/1000
forecasts$pop2020=forecasts$PopTotal2020/1000

forecasts$BaseHC2030=as.numeric(forecasts$BaseHC2030)
forecasts$basepoorpop=(forecasts$BaseHC2030*forecasts$pop2030)

forecasts$PlusOneHC2030=as.numeric(forecasts$PlusOneHC2030)
forecasts$plusonepop=(forecasts$PlusOneHC2030*forecasts$pop2030)
forecasts$MinusOneHC2030=as.numeric(forecasts$MinusOneHC2030)
forecasts$minusonepop=(forecasts$MinusOneHC2030*forecasts$pop2030)
forecasts$basepoorpop2020=forecasts$BaseHC2020*forecasts$pop2020

# otherfactors=read.csv("C:/Users/Zach/Documents/NCLB final/oldrankings2018StatesofFrag.csv")
# StatesofFrag=otherfactors[,c("Short.Name","OECD.Fragility.2rank")]
# StatesofFrag$CountryName=unfactor(StatesofFrag$Short.Name)
# StatesofFrag=StatesofFrag[,c("CountryName","OECD.Fragility.2rank")]
# names(StatesofFrag)=c("CountryName","OECD.Fragility.Rank")
# 
# 
# 
# 
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="The Bahamas")]="Bahamas"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Brunei")]="Brunei Darussalam"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Congo")]="Congo, Republic of"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Côte d'Ivoire")]="Cote d'Ivoire"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Lao PDR")]="Lao People's Democratic Republic"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Dem. Rep. Congo")]="Congo, Democratic Republic of"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Egypt")]="Egypt, Arab Republic of"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="The Gambia")]="Gambia, The"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Iran")]="Iran, Islamic Republic of"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Russia")]="Russian Federation"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="São Tomé and Principe")]="Sao Tome and Principe"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Venezuela")]="Venezuela, Republica Bolivariana de"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Yemen")]="Yemen, Republic of"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Macedonia")]="Macedonia, former Yugoslav Republic of"
# StatesofFrag$CountryName[which(StatesofFrag$CountryName=="Korea")]="Korea, Republic of"
# 
# 
# setdiff(StatesofFrag$CountryName,forecasts$CountryName)
# setdiff(forecasts$CountryName,StatesofFrag$CountryName)
# 
# write.csv(StatesofFrag,"data/StatesofFragility2018.csv",row.names = F,na="")

statesoffrag=read.csv("data/StatesofFragility2018.csv")
statesoffrag$frag=0
statesoffrag$frag[which(statesoffrag$OECD.Fragility.Rank<57)]=1
statesoffrag=statesoffrag[,c("CountryName","frag")]

forecasts=join(forecasts,statesoffrag,by=c("CountryName"))

data.table(forecasts)[,.(
  poorpop2020=sum(basepoorpop2020, na.rm=T)
),by=c("frag")]

forecasts2020=forecasts[,c("CountryName","basepoorpop2020","frag")]

write.csv(forecasts2020,"data/poorpops_2020.csv", row.names=F,na="")
write.csv(forecasts,"data/WEOpoverty_forecasts.csv",row.names=F,na="")
