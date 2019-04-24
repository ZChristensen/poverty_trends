
list.of.packages <- c("data.table","readr","Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd="E:/git/poverty_trends"
setwd(wd)

load("data/SMYPovcalScrape6April2019.RData")
smy190=subset(smy, PovertyLine==1.9)
smy190=unique(smy190)
smy1902015=smy190[which(smy190$RequestYear==2015),]
smy1901999=smy190[which(smy190$RequestYear==1999),]
smy1901999$consumptionfloor99=smy1901999$PovertyLine*(1-(smy1901999$PovGapSqr/smy1901999$PovGap))
smy1902015$consumptionfloor15=smy1902015$PovertyLine*(1-(smy1902015$PovGapSqr/smy1902015$PovGap))
smy1901999=smy1901999[,c("CountryName","HeadCount", "consumptionfloor99")]
names(smy1901999)=c("CountryName","HeadCount99", "consumptionfloor99")
smys=merge(smy1902015,smy1901999,by=c("CountryName"))
mys$povdiff=smys$HeadCount99-smys$HeadCount
smys$consumptionfloordiff=smys$consumptionfloor15-smys$consumptionfloor99
comparisons=smys[which(smys$CountryName %in% c("Ethiopia","Uzbekistan")),]
comparisons99=smys[,c("CountryName","HeadCount99","consumptionfloor99")]
comparisons15=smys[,c("CountryName","HeadCount","consumptionfloor15")]
setnames(comparisons15,"consumptionfloor15","consumptionfloor")
setnames(comparisons99,"consumptionfloor99","consumptionfloor")
setnames(comparisons99,"HeadCount99","HeadCount")
comparisons15$year=2015
comparisons99$year=1999
comp=rbind(comparisons15,comparisons99)
ggplot(data=comp[which(comp$CountryName %in% c("Ethiopia","Uzbekistan")),], aes(x=year, y=HeadCount, group=CountryName,color=CountryName))+
  geom_line()+
  geom_point()
ggplot(data=comp[which(comp$CountryName %in% c("Ethiopia","Uzbekistan")),], aes(x=year, y=consumptionfloor, group=CountryName,color=CountryName))+
  geom_line()+
  geom_point()
