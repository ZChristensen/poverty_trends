#Assessing updates to PovcalNet
list.of.packages <- c("Hmisc","plyr","data.table","varhandle","ggplot2","stringr","httr","jsonlite","excel.link")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
#Loading PovcalNet data

load("C:/Users/Zach/Documents/Poverty data/SMYPovcalScrape1May2018.RData")
load("C:/Users/Zach/Documents/Poverty data/AGGPovcalScrape1May2018.RData")

agg_total=unfactor(data.frame(agg_total))
                  regional.extpov = subset(agg_total, PovertyLine==1.90)
                  regional.threetwenty=subset(agg_total, PovertyLine==3.20)
                  regional.fivefifty=subset(agg_total, PovertyLine==5.50)
                  
old_regional=rbind(regional.extpov,regional.threetwenty,regional.fivefifty)
old_regional$P2=as.numeric(old_regional$P2)
old_regional$PG=as.numeric(old_regional$PG)
old_regional$PovertyLine=as.numeric(old_regional$PovertyLine)
old_regional$ConsumptionFloor = old_regional$PovertyLine*(1-(old_regional$P2/old_regional$PG))
smy_total=unfactor(data.frame(smy_total))
smy_total= subset(smy_total, displayMode==0|displayMode==2|displayMode==4|displayMode==5)
      smy.extpov=subset(smy_total,PovertyLine==1.90)
      smy.threetwenty=subset(smy_total,PovertyLine==3.2)
      smy.fivefifty=subset(smy_total,PovertyLine==5.5)

old_smy=rbind(smy.extpov,smy.threetwenty,smy.fivefifty)


#Loading PovcalNet data

url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx"
params = list(
  "Countries"="all",
  "GroupedBy"="WB",
  "PovertyLine"="1.90",
  "YearSelected"=paste0(c(1981:2015),collapse=","),
  "format"="csv"
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
for(povline in c(1.9,3.2,5.5)){
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
}
agg_total=rbindlist(agg_results)
smy_total=rbindlist(smy_results)



smy_total= subset(smy_total, displayMode==0|displayMode==2|displayMode==4|displayMode==5)

# 
# #
# list_index=1
# # for(povline in c(1.9,3.2,5.5)){
#   message(povline)
#   params["PovertyLine"]=povline
#   response=POST(url,body=params)
#   result=content(response)
#   agg_matches = regmatches(result,gregexpr('aggrItem\\((.*?)\\)',result,perl=T))[[1]]
#   smy_matches = regmatches(result,gregexpr('smyItem\\((.*?)\\)',result,perl=T))[[1]]
#   agg_matches = gsub(")","",substr(agg_matches,10,nchar(agg_matches)))
#   smy_matches = gsub(")","",substr(smy_matches,9,nchar(smy_matches)))
#   smy_matches = gsub("[","",smy_matches,fixed=T)
#   smy_matches = gsub("]","",smy_matches,fixed=T)
#   agg_match_str = paste0("[",paste(paste0("[",agg_matches,"]"),collapse=","),"]")
#   smy_match_str = paste0("[",paste(paste0("[",smy_matches,"]"),collapse=","),"]")
#   agg=data.frame(fromJSON(agg_match_str))
#   smy_t = lapply(fromJSON(smy_match_str),t)
#   smy_dfs=lapply(smy_t,data.frame)
#   smy=rbindlist(smy_dfs,fill=T)
#   names(agg)=c("RequestYear","RegionTitle","RegionCID","PovertyLine","Mean","H","PG","P2","Populations")
#   names(smy)=c("isConsolidated","displayMode","useMicroData","CountryCode","CountryName","RegionCID","CoverageType","RequestYear","DataType","PPP","PovertyLine","Mean","H","PG","P2","watts","gini","median","mld","pol","rmed","rmhalf","ris","IA","Populations","DataYear","SvyInfoID","PPPStatus","Decile1","Decile2","Decile3","Decile4","Decile5","Decile6","Decile7","Decile8","Decile9","Decile10","Unknown")
#   agg_results[[list_index]] = agg
#   smy_results[[list_index]] = smy
#   list_index=list_index+1
# }
# 
# agg_total=rbindlist(agg_results)
# smy_total=rbindlist(smy_results)
# wd="E:/git/poverty_trends"
# setwd(wd)
# save(smy_total,file="SMYPovcalScrapeFall2018.RData")
# save(agg_total,file="AGGPovcalScrapeFall2018.RData")


setdiff(smy_total$CountryName,old_smy$CountryName)
setdiff(old_smy$CountryName,smy_total$CountryName)


#Global Check
setdiff(old_regional$RequestYear,agg_total$RequestYear)
setdiff(agg_total$RequestYear,old_regional$RequestYear)

setnames(old_regional,"H","oldH")
setnames(old_regional,"Populations","oldPopulations")
setnames(old_regional,"ConsumptionFloor","oldConsumptionFloor")
setnames(old_regional,"Mean","oldMean")
regions=join(agg_total,old_regional, by=c("RequestYear","RegionCID","PovertyLine"))


#Regional Check


#Country Check

