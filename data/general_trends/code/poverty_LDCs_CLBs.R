#Income gaps
list.of.packages <- c("data.table","readr","Hmisc","varhandle","plyr","reshape2","xlsx","WDI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

source("https://raw.githubusercontent.com/akmiller01/alexm-util/master/DevInit/R/fuzzy_match.R")

setwd("E:/git/poverty_trends/data")
UNcategories=read.xlsx("general_trends/input/country categories.xlsx","Summary")

P20trends=fread("P20incometrends.csv")


# dictionary=fuzzy(unique(P20trends$CountryName),unique(UNcategories$country))
# dictionary$y=unfactor(dictionary$y)
# dictionary$y[which(dictionary$x=="Lao People's Democratic Republic")]="Lao PDR"
# setnames(dictionary, "x","CountryName")
# setnames(dictionary,"y","country")
# fwrite(dictionary,"UNnamestoPovcalnames.csv")
dictionary=fread("UNnamestoPovcalnames.csv")
P20trends=join(P20trends,dictionary,by=c("CountryName"))
UNcategories=UNcategories[,c("country","income.group","disaggregated.income.group")]
P20trends=join(P20trends,UNcategories,by=c("country"))

CLB=c(
  "Afghanistan"
  ,"Benin"
  ,"Burundi"
  ,"Central African Republic"
  ,"Chad"
  ,"Congo, Republic of"
  ,"Congo, Democratic Republic of"
  ,"Eritrea"
  ,"Gambia, The"
  ,"Guinea"
  ,"Guinea-Bissau"
  ,"Haiti"
  ,"Lesotho"
  ,"Liberia"
  ,"Madagascar"
  ,"Malawi"
  ,"Mali"
  ,"Micronesia, Federated States of"
  ,"Mozambique"
  ,"Niger"
  ,"Nigeria"
  ,"Papua New Guinea"
  ,"Somalia"
  ,"South Sudan"
  ,"Sudan"
  ,"Syrian Arab Republic"
  ,"Togo"
  ,"Uganda"
  ,"Yemen, Republic of"
  ,"Zambia"
  )

P20trends$CLB=0
P20trends$CLB[which(P20trends$CountryName %in% CLB)]=1
total.poor.pop=data.table(P20trends)[,.(
  total.poor.pop=sum(pop*ExtPovHC)
),by=c("RequestYear")]


povertybyyear=data.table(P20trends)[,.(
  group.poor.pop=sum(ExtPovHC*pop)
),by=c("RequestYear","income.group")]
povertybyyear=join(povertybyyear,total.poor.pop,by=c("RequestYear"))
povertybyyear$share.of.poor=povertybyyear$group.poor.pop/povertybyyear$total.poor.pop
povertybyyear=subset(povertybyyear, RequestYear %in% c(2010,2015))

fwrite(povertybyyear,"shareofpoorpopbyUNIncome.csv")



povertybyyear.clb=data.table(P20trends)[,.(
  group.poor.pop=sum(ExtPovHC*pop)
),by=c("RequestYear","CLB")]
povertybyyear.clb=join(povertybyyear.clb,total.poor.pop,by=c("RequestYear"))
povertybyyear.clb$share.of.poor=povertybyyear.clb$group.poor.pop/povertybyyear.clb$total.poor.pop
povertybyyear.clb=subset(povertybyyear.clb, RequestYear %in% c(2010,2015))
fwrite(povertybyyear.clb,"shareofpoorpopbyCLB.csv")
