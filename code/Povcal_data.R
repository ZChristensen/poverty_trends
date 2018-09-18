list.of.packages <- c("stringr","httr","jsonlite","varhandle","plyr","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


#Loading PovcalNet data

url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx"
params = list(
  "Countries"="all",
  "GroupedBy"="WB",
  "PovertyLine"="1.90",
  "YearSelected"=paste0(c(1981:2013),collapse=","),
  "format"="js"
)
agg_results = list()
smy_results = list()

init_c_item_lines = readLines("http://iresearch.worldbank.org/PovcalNet/js/initCItem2014.js")
init_c_item_lines = init_c_item_lines[which(grepl("cItem",init_c_item_lines))]
c_items = substr(init_c_item_lines,15,19)

for(i in 1:length(c_items)){
  c_item = c_items[i]
  zero_index = i-1
  param_name = paste0("C",zero_index)
  params[param_name] = c_item
}

#
list_index=1
povline=1.90
# for(povline in c(seq(from=0,to=10,by=0.01),seq(from=10,to=25.5,by=0.25),seq(from=25.5,to=35.5,by=0.025),seq(from=35,to=500,by=1),seq(from=500,to=800,by=5))){
  message(povline)
  params["PovertyLine"]=povline
  response=POST(url,body=params)
  result=content(response)
  agg_matches = regmatches(result,gregexpr('aggrItem\\((.*?)\\)',result,perl=T))[[1]]
  smy_matches = regmatches(result,gregexpr('smyItem\\((.*?)\\)',result,perl=T))[[1]]
  agg_matches = gsub(")","",substr(agg_matches,10,nchar(agg_matches)))
  smy_matches = gsub(")","",substr(smy_matches,9,nchar(smy_matches)))
  smy_matches = gsub("[","",smy_matches,fixed=T)
  smy_matches = gsub("]","",smy_matches,fixed=T)
  agg_match_str = paste0("[",paste(paste0("[",agg_matches,"]"),collapse=","),"]")
  smy_match_str = paste0("[",paste(paste0("[",smy_matches,"]"),collapse=","),"]")
  agg=data.frame(fromJSON(agg_match_str))
  smy_t = lapply(fromJSON(smy_match_str),t)
  smy_dfs=lapply(smy_t,data.frame)
  smy=rbindlist(smy_dfs,fill=T)
  names(agg)=c("RequestYear","RegionTitle","RegionCID","PovertyLine","Mean","H","PG","P2","Populations")
  names(smy)=c("isConsolidated","displayMode","useMicroData","CountryCode","CountryName","RegionCID","CoverageType","RequestYear","DataType","PPP","PovertyLine","Mean","H","PG","P2","watts","gini","median","mld","pol","rmed","rmhalf","ris","IA","Populations","DataYear","SvyInfoID","PPPStatus","Decile1","Decile2","Decile3","Decile4","Decile5","Decile6","Decile7","Decile8","Decile9","Decile10","Unknown")
  agg_results[[list_index]] = agg
  smy_results[[list_index]] = smy
  list_index=list_index+1
# }

agg_total=rbindlist(agg_results)
smy_total=rbindlist(smy_results)
wd="C:/Users/Zach/Documents/Poverty data"
setwd(wd)
save(smy_total,file="SMYPovcalScrape1May2018.RData")
save(agg_total,file="AGGPovcalScrape1May2018.RData")
load("C:/Users/Zach/Documents/Poverty data/SMYPovcalScrape1May2018.RData")
load("C:/Users/Zach/Documents/Poverty data/AGGPovcalScrape1May2018.RData")
wd="C:/Users/Zach/Documents/Poverty data"
setwd(wd)
smy_total=unfactor(data.frame(smy_total))
smy_total= subset(smy_total, CoverageType==3|CoverageType==5)
smy_total= smy_total[,1:28]





#This code calculates p20 threshold
#https://github.com/akmiller01/alexm-util/blob/master/DevInit/datahub_auto/povcal_calc.py



agg_total=unfactor(data.frame(agg_total))
agg_total$P2 = as.numeric(agg_total$P2)
agg_total$PG = as.numeric(agg_total$PG)
agg_total$H = as.numeric(agg_total$H)
agg_total$ConsumptionFloor = agg_total$PovertyLine*(1-(agg_total$P2/agg_total$PG))
agg_total$diff=abs(agg_total$H-20)
regional.extpov = subset(agg_total, PovertyLine==1.90)
GlobalExtPov = subset(regional.extpov, RegionTitle=="World Total")
names(GlobalExtPov)[which(names(GlobalExtPov)=="ConsumptionFloor")] <- "Global.Consumption.Floor"
names(GlobalExtPov)[which(names(GlobalExtPov)=="H")] <- "Global.Ext.HC"
keep=c("RequestYear","Global.Consumption.Floor","Global.Ext.HC")
GlobalExtPov=GlobalExtPov[,keep]
regional.p20 = data.table(agg_total)[,.SD[which.min(diff)],by=.(RegionTitle,RequestYear)]
WorldP20threshold = subset(regional.p20, RegionTitle=="World Total")
WorldP20threshold$P20Threshold = WorldP20threshold$PovertyLine
keep=c("RequestYear","P20Threshold")
WorldP20threshold2=data.frame(WorldP20threshold)[,keep]


smy_total= subset(smy_total, displayMode==0|displayMode==2|displayMode==4|displayMode==5)
smy_total$P2 = as.numeric(smy_total$P2) 
smy_total$PG = as.numeric(smy_total$PG)
smy_total$ConsumptionFloor= smy_total$PovertyLine*(1-(smy_total$P2/smy_total$PG))
smy_total$H = as.numeric(smy_total$H)
smy_total$diff = abs(smy_total$H - 20)
smy_total=join(smy_total, WorldP20threshold2, by="RequestYear")
smy_P20 = subset(smy_total, PovertyLine==P20Threshold)
names(smy_P20)[which(names(smy_P20)=="H")] <- "P20Headcount"
keep=c("CountryName","RequestYear","PovertyLine","H","CountryCode","ConsumptionFloor")
smy_total2=smy_total[,keep]
countries.np20 = data.table(smy_total)[,.SD[which.min(diff)],by=.(CountryCode,RequestYear)]
names(countries.np20)[which(names(countries.np20)=="PG")] <- "NP20PG"
keep=c("CountryName","RequestYear","PovertyLine","H","CountryCode","NP20PG")
countries.np20=data.frame(countries.np20)[,keep]
colnames(countries.np20)[colnames(countries.np20)=="PovertyLine"] <-"NP20.Threshold"
smy_extremepov = subset(smy_total2,PovertyLine==1.90)
names(smy_extremepov)[which(names(smy_extremepov)=="H")] <- "ExtPovHC"
smy_LMpov = subset(smy_total2,PovertyLine==3.20)
names(smy_LMpov)[which(names(smy_LMpov)=="H")] <- "LMPovHC"
smy_UMpov = subset(smy_total2,PovertyLine==5.50)
names(smy_UMpov)[which(names(smy_UMpov)=="H")] <- "UMPovHC"

dfs <- list(smy_P20,smy_extremepov,smy_LMpov,smy_UMpov,countries.np20)
P20main<- join_all(dfs,by=c("RequestYear","CountryCode","CountryName"))

rm("smy_P20","smy_extremepov","smy_LMpov","smy_UMpov","countries.np20")
gc()


#Calculate averages of P20 and rest
P20main= data.frame(P20main)
P20main$Populations = gsub(",","",P20main$Populations)
P20main$pop = as.numeric((P20main$Populations)) * 1000000
P20main$P20pop = (P20main$P20Headcount/100)*(P20main$pop)
P20main$P20average = P20main$PovertyLine -((P20main$PovertyLine*(P20main$PG/100)*P20main$pop)/P20main$P20pop)
P20main$restpop = P20main$pop - P20main$P20pop
P20main$restaverage = (((P20main$Mean/(365/12))*P20main$pop)-(P20main$P20average * P20main$P20pop))/P20main$restpop

P20main= data.frame(P20main)
P20main$Populations = gsub(",","",P20main$Populations)
P20main$pop = as.numeric((P20main$Populations)) * 1000000
P20main$P20pop = (P20main$P20Headcount/100)*(P20main$pop)
P20main$P20average = P20main$PovertyLine -((P20main$PovertyLine*(P20main$PG/100)*P20main$pop)/P20main$P20pop)
P20main$restpop = P20main$pop - P20main$P20pop
P20main$restaverage = (((P20main$Mean/(365/12))*P20main$pop)-(P20main$P20average * P20main$P20pop))/P20main$restpop

#Calculate averages of NP20 and rest
P20main= data.frame(P20main)
P20main$Populations = gsub(",","",P20main$Populations)
P20main$pop = as.numeric((P20main$Populations)) * 1000000
P20main$NP20pop = P20main$pop * .20
P20main$NP20average = P20main$PovertyLine -((P20main$NP20.Threshold*(P20main$NP20PG/100)*P20main$pop)/P20main$NP20pop)
P20main$restpop = P20main$pop - P20main$NP20pop
P20main$Nrestaverage = (((P20main$Mean/(365/12))*P20main$pop)-(P20main$NP20average * P20main$NP20pop))/P20main$restpop



P20main=join(P20main, GlobalExtPov, by="RequestYear")


write.csv(P20main,"P20incometrends20180521.csv",row.names=FALSE,na="")

