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
                  regional.extpov$ConsumptionFloor = regional.extpov$PovertyLine*(1-(regional.extpov$P2/regional.extpov$PG))
                  regional.threetwenty=subset(agg_total, PovertyLine==3.20)
                  regional.fivefifty=subset(agg_total, PovertyLine==5.50)
                  
old_regional=rbind(regional.extpov,regional.threetwenty,regional.fivefifty)
old_regional$ConsumptionFloor = old_regional$PovertyLine*(1-(old_regional$P2/old_regional$PG))


# url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx"
# params = list(
#   "C0"="ALB_3",
#   "C1"="DZA_3",
#   "C2"="AGO_3",
#   "C3"="AGO_2",
#   "C4"="ARG_2",
#   "C5"="ARM_3",
#   "C6"="AUS_3",
#   "C7"="AUT_3",
#   "C8"="AZE_3",
#   "C9"="BGD_3",
#   "C10"="BLR_3",
#   "C11"="BEL_3",
#   "C12"="BLZ_3",
#   "C13"="BEN_3",
#   "C14"="BTN_3",
#   "C15"="BOL_3",
#   "C16"="BOL_2",
#   "C17"="BIH_3",
#   "C18"="BWA_3",
#   "C19"="BRA_3",
#   "C20"="BGR_3",
#   "C21"="BFA_3",
#   "C22"="BDI_3",
#   "C23"="CPV_3",
#   "C24"="CMR_3",
#   "C25"="CAN_3",
#   "C26"="CAF_3",
#   "C27"="TCD_3",
#   "C28"="CHL_3",
#   "C29"="CHN_5",
#   "C30"="CHN_1",
#   "C31"="CHN_2",
#   "C32"="COL_3",
#   "C33"="COL_2",
#   "C34"="COM_3",
#   "C35"="ZAR_3",
#   "C36"="COG_3",
#   "C37"="CRI_3",
#   "C38"="CIV_3",
#   "C39"="HRV_3",
#   "C40"="CYP_3",
#   "C41"="CZE_3",
#   "C42"="DNK_3",
#   "C43"="DJI_3",
#   "C44"="DOM_3",
#   "C45"="ECU_3",
#   "C46"="ECU_2",
#   "C47"="EGY_3",
#   "C48"="SLV_3",
#   "C49"="EST_3",
#   "C50"="ETH_3",
#   "C51"="ETH_1",
#   "C52"="FJI_3",
#   "C53"="FIN_3",
#   "C54"="FRA_3",
#   "C55"="GAB_3",
#   "C56"="GMB_3",
#   "C57"="GEO_3",
#   "C58"="DEU_3",
#   "C59"="GHA_3",
#   "C60"="GRC_3",
#   "C61"="GTM_3",
#   "C62"="GNB_3",
#   "C63"="GIN_3",
#   "C64"="GUY_3",
#   "C65"="HTI_3",
#   "C66"="HND_3",
#   "C67"="HND_2",
#   "C68"="HUN_3",
#   "C69"="ISL_3",
#   "C70"="IND_5",
#   "C71"="IND_1",
#   "C72"="IND_2",
#   "C73"="IDN_5",
#   "C74"="IDN_1",
#   "C75"="IDN_2",
#   "C76"="IRN_3",
#   "C77"="IRQ_3",
#   "C78"="IRL_3",
#   "C79"="ISR_3",
#   "C80"="ITA_3",
#   "C81"="JAM_3",
#   "C82"="JPN_3",
#   "C83"="JOR_3",
#   "C84"="KAZ_3",
#   "C85"="KEN_3",
#   "C86"="KIR_3",
#   "C87"="KOR_3",
#   "C88"="KSV_3",
#   "C89"="KGZ_3",
#   "C90"="LAO_3",
#   "C91"="LVA_3",
#   "C92"="LBN_3",
#   "C93"="LSO_3",
#   "C94"="LBR_3",
#   "C95"="LTU_3",
#   "C96"="LUX_3",
#   "C97"="MKD_3",
#   "C98"="MDG_3",
#   "C99"="MWI_3",
#   "C100"="MYS_3",
#   "C101"="MDV_3",
#   "C102"="MLI_3",
#   "C103"="MLT_3",
#   "C104"="MRT_3",
#   "C105"="MUS_3",
#   "C106"="MEX_3",
#   "C107"="FSM_3",
#   "C108"="FSM_2",
#   "C109"="MDA_3",
#   "C110"="MNG_3",
#   "C111"="MNE_3",
#   "C112"="MAR_3",
#   "C113"="MOZ_3",
#   "C114"="MMR_3",
#   "C115"="NAM_3",
#   "C116"="NPL_3",
#   "C117"="NLD_3",
#   "C118"="NIC_3",
#   "C119"="NER_3",
#   "C120"="NGA_3",
#   "C121"="NOR_3",
#   "C122"="PAK_3",
#   "C123"="PAN_3",
#   "C124"="PNG_3",
#   "C125"="PRY_3",
#   "C126"="PER_3",
#   "C127"="PHL_3",
#   "C128"="POL_3",
#   "C129"="PRT_3",
#   "C130"="ROU_3",
#   "C131"="RUS_3",
#   "C132"="RWA_3",
#   "C133"="WSM_3",
#   "C134"="STP_3",
#   "C135"="SEN_3",
#   "C136"="SRB_3",
#   "C137"="SYC_3",
#   "C138"="SLE_3",
#   "C139"="SVK_3",
#   "C140"="SVN_3",
#   "C141"="SLB_3",
#   "C142"="ZAF_3",
#   "C143"="SSD_3",
#   "C144"="ESP_3",
#   "C145"="LKA_3",
#   "C146"="LCA_3",
#   "C147"="SDN_3",
#   "C148"="SUR_3",
#   "C149"="SWZ_3",
#   "C150"="SWE_3",
#   "C151"="CHE_3",
#   "C152"="SYR_3",
#   "C153"="TJK_3",
#   "C154"="TZA_3",
#   "C155"="THA_3",
#   "C156"="TMP_3",
#   "C157"="TGO_3",
#   "C158"="TON_3",
#   "C159"="TTO_3",
#   "C160"="TUN_3",
#   "C161"="TUR_3",
#   "C162"="TKM_3",
#   "C163"="TUV_3",
#   "C164"="UGA_3",
#   "C165"="UKR_3",
#   "C166"="GBR_3",
#   "C167"="USA_3",
#   "C168"="URY_3",
#   "C169"="URY_2",
#   "C170"="UZB_3",
#   "C171"="VUT_3",
#   "C172"="VEN_3",
#   "C173"="VNM_3",
#   "C174"="WBG_3",
#   "C175"="YEM_3",
#   "C176"="ZMB_3",
#   "C177"="ZWE_3",
#   "Countries"="all",
#   "GroupedBy"="WB",
#   "PovertyLine"="1.90",
#   "YearSelected"=paste0(c(1981:2013),collapse=","),
#   "format"="js"
# )
# agg_results = list()
# smy_results = list()
# 
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



#Global Check
#Regional Check
#Country Check