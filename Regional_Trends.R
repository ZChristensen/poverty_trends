list.of.packages <- c("data.table","readr","varhandle","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

load("C:/Users/Zach/Documents/Poverty data/AGGPovcalScrape1May2018.RData")

agg_total=agg_total[which(agg_total$PovertyLine==1.90 & unfactor(agg_total$RequestYear)>1998&agg_total$RegionTitle!="World Total"),]
agg_total$pop_in_pov=(unfactor(agg_total$H)/100)*(unfactor(agg_total$Populations))
ggplot(agg_total, aes(x=RequestYear, y=pop_in_pov, group=RegionTitle, color=RegionTitle))+geom_line()
agg_total=agg_total[,c("RequestYear","RegionTitle","pop_in_pov")]
write.csv(agg_total,"E:/git/poverty_trends/regional_poverty_trends.csv",row.names = F, na="")
