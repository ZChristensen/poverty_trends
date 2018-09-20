list.of.packages <- c("stringr","httr","jsonlite","varhandle","plyr","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


povcal_smy = function(pl=1.9){
  url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  params = list(
    "Countries"="all",
    "PovertyLine"=as.character(pl),
    "SurveyYears"="all",
    "Display"="C",
    "format"="csv"
  )
  
  param_names = names(params)
  for(param_name in param_names){
    param = params[[param_name]]
    url = paste0(url,param_name,"=",param,"&")
  }
  # Remove the last &
  url = substr(url,1,nchar(url)-1)
  return(read.csv(url))
}

# Default group by "WB" can also be "UN" or "Income"
povcal_agg = function(pl=1.9,group.by="WB"){
  url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  params = list(
    "Countries"="all",
    "PovertyLine"=as.character(pl),
    "RefYears"="all",
    "Display"="Regional",
    "GroupedBy"=group.by,
    "format"="csv"
  )
  
  param_names = names(params)
  for(param_name in param_names){
    param = params[[param_name]]
    url = paste0(url,param_name,"=",param,"&")
  }
  # Remove the last &
  url = substr(url,1,nchar(url)-1)
  return(read.csv(url))
}
