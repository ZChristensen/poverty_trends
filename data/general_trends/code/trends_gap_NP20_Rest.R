
list.of.packages <- c("data.table","readr","Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd="E:/git/poverty_trends"
setwd(wd)

P20trends=fread("data/P20incometrends.csv")

P20trends$gap=P20trends$Nrestaverage-P20trends$NP20average
P20trends1981=subset(P20trends, RequestYear %in% c(1990))
P20trends2015=subset(P20trends, RequestYear %in% c(2015))
P20trends2=merge(P20trends1981,P20trends2015,by=c("CountryName"))
P20trends2$growing.gap.NP20.rest=NA
P20trends2$growing.gap.NP20.rest[which(P20trends2$gap.y>P20trends2$gap.x)]="increasing"
P20trends2$growing.gap.NP20.rest[which(P20trends2$gap.y<P20trends2$gap.x)]="decreasing"

P20trends2=P20trends2[,c("CountryName","growing.gap.NP20.rest")]
UNcategories=read.xlsx("E:/git/poverty_trends/data/general_trends/input/country categories.xlsx","Summary")
dictionary=fread("E:/git/poverty_trends/data/UNnamestoPovcalnames.csv")
P20trends2=join(P20trends2,dictionary,by=c("CountryName"))
UNcategories$LDC=0
UNcategories$LDC[which(UNcategories$TRUE.if.country.is.an.LDC==T)]=1
UNcategories=UNcategories[,c("country","income.group","disaggregated.income.group","LDC")]
P20trends3=join(P20trends2,UNcategories,by=c("country"))

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

P20trends3$CLB=0
P20trends3$CLB[which(P20trends3$CountryName %in% CLB)]=1
table(P20trends3$growing.gap.NP20.rest,P20trends3$CLB)

table(P20trends3$growing.gap.NP20.rest,P20trends3$LDC)


