#Assessing updates to PovcalNet
list.of.packages <- c("plyr","data.table","varhandle","ggplot2","reshape2","readr")
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



#Loading PovcalNet data
load("data/AGGPovcalScrapeSept2018.RData")



#Elephant graph
masterlist=list()
masterlist.index=1
for (i in
     c(seq(.01,.99,by=.01),
       seq(.991,1,by=.001)
     )){
  print(i)
  new=data.table(agg_total[which(agg_total$regionCID=="WLD")])[,.SD[which.min(abs(hc-i))],by=c("requestYear")]
  masterlist[[masterlist.index]]=new
  masterlist.index=masterlist.index+1
}
master=rbindlist(masterlist)
master=master[,c("hc","povertyLine","requestYear","regionCID")]

master=master[order(master$requestYear, master$hc),]
master$HC=c(seq(1,99,by=1),
            seq(99.1,100,by=.1))

years=c(unique(master$requestYear))
years=years[order(years)]
headcounts=unique(master$hc)
countries=unique(master$regionCID)

master=data.frame(master)

if(exists("masterwide")){
  rm(masterwide)
}
for(year in years){ 
  year.values=c()
  year.hcs=c()
  year.nam=paste0("Income",year)
  year.values=subset(master, requestYear==year)[c("HC","povertyLine")]
  setnames(year.values,"povertyLine",year.nam)
  if(exists("masterwide")){
    masterwide=merge(masterwide,year.values,by="HC",all=T)
  }
  else{
    masterwide=year.values
  }
}


masterwide$growth87to08rt = ((masterwide$Income2008-masterwide$Income1987)/masterwide$Income1987)/(2008-1987)
masterwide$growth03to15rt = ((masterwide$Income2015-masterwide$Income2002)/masterwide$Income2002)/(2015-2002)
masterwide$growth93to15rt = ((masterwide$Income2015-masterwide$Income1993)/masterwide$Income1993)/(2015-1993)
masterwide$growth08to15rt = ((masterwide$Income2015-masterwide$Income2008)/masterwide$Income2008)/(2015-2008)
masterwide$growth13to15rt = ((masterwide$Income2015-masterwide$Income2013)/masterwide$Income2013)/(2015-2013)
ggplot(masterwide[which(masterwide$HC!=100),],aes(x=HC))+
  geom_line(aes(x=HC,y=growth13to15rt))+
  # geom_line(aes(x=HC,y=growth03to15rt))+
  # geom_line(aes(x=HC,y=growth93to15rt))+
  # geom_line(aes(x=HC,y=growth08to15rt))+
  labs(x="Global Income Percentile",y="Income growth rate\n2013-2015")+
  scale_y_continuous(labels= scales::percent)+
  theme_classic()
elephant=masterwide[,c("HC","growth13to15rt")] 
elephant=elephant[which(elephant$HC!=100),]
write.csv(elephant,"data/elephant_curve_13to15.csv",row.names=F,na="")
