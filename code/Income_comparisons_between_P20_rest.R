#Income gaps
list.of.packages <- c("data.table","excel.link","ggplot2","readr","stringr","scales","Hmisc")
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


P20main <- read.csv("data/P20incometrends.csv")
P20main$Consumption=NA
P20main$Consumption[which(P20main$DataType==0)]="Consumption"
P20main$Consumption[which(P20main$DataType==1)]="Income"

P20main$RegionCID=NA
P20main$RegionCID[which(P20main$CountryName %in%
                          c(
                            "Albania"
                            ,"Armenia"
                            ,"Azerbaijan"
                            ,"Belarus"
                            ,"Bulgaria"
                            ,"Croatia"
                            ,"Czech Republic"
                            ,"Estonia"
                            ,"Georgia"
                            ,"Hungary"
                            ,"Kazakhstan"
                            ,"Kyrgyz Republic"
                            ,"Latvia"                                 Lithuania                             
                            [15] Poland                                 Romania                               
                            [17] Russian Federation                     Slovak Republic                       
                            [19] Turkey                                 Ukraine                               
                            [21] Uzbekistan                             Bosnia and Herzegovina                
                            [23] Macedonia, former Yugoslav Republic of Moldova                               
                            [25] Montenegro                             Serbia                                
                            [27] Slovenia                               Tajikistan                            
                            [29] Turkmenistan                           Kosovo 
                          ))]

P20main2=P20main[,c(
                    "CountryName"
                    ,"RequestYear"
                    ,"P20average"
                    ,"restaverage"
                    ,"P20Headcount"
                    ,"P20pop"
                    ,"restpop"
                    ,"ExtPovHC"
                    ,"RegionCID"
                    ,"Consumption"
                    ,"NP20average"
                    ,"Nrestaverage"
)]
names(P20main2)[which(names(P20main2)=="P20Headcount")] <- "Percent.in.P20"
names(P20main2)[which(names(P20main2)=="ExtPovHC")] <- "Percent.in.Extreme.Poverty"
names(P20main2)[which(names(P20main2)=="RequestYear")] <- "Year"


regions=unique(P20main$RegionCID)


for(region in regions){
  print(region)
  filenam=paste0("C:/Users/Zach/Documents/P20 Analysis 2018/Income comparisons P20 and rest/P20versusRestIncomeTrends20180602",region,".xlsx")
  P20main3=subset(P20main2, RegionCID==region)
  xl.workbook.add()
  countries=unique(P20main3$CountryName)
  for(country in countries){
    new=subset(P20main3, CountryName==country)
    max=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("CountryName","P20average","restaverage","Year")]
    names(new)=c("CountryName","P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("CountryName","Year"))
    title=paste(country, "\n",Consumption,"Trends")
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+
      geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+
      theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,max),labels=dollar)
      # coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.sheet.delete("Sheet1")
  xl.workbook.save(filenam)
  xl.workbook.close()
}

for(region in regions){
  print(region)
  filenam=paste0("C:/Users/Zach/Documents/P20 Analysis 2018/Income comparisons P20 and rest/NP20versusRestIncomeTrends20180602",region,".xlsx")
  P20main3=subset(P20main2, RegionCID==region)
  xl.workbook.add()
  countries=unique(P20main3$CountryName)
  for(country in countries){
    new=subset(P20main3, CountryName==country)
    max=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("CountryName","NP20average","Nrestaverage","Year")]
    names(new)=c("CountryName","National P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("CountryName","Year"))
    title=paste(country, "\n",Consumption,"Trends")
    # p=ggplot(data=new, aes(x=Year, y=Nrestaverage))+ geom_line(aes(y=NP20average, colour="National P20"))+ geom_line(aes(y=Nrestaverage, colour="Rest"))+labs(title=title,y=paste("Average daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year")+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+scale_y_continuous(labels=dollar)
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,max),labels=dollar)
    # +coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.sheet.delete("Sheet1")
  xl.workbook.save(filenam)
  xl.workbook.close()
}

#regions
for(region in regions){
  print(region)
  filenam=paste0("C:/Users/Zach/Documents/P20 Analysis 2018/Income comparisons P20 and rest/NP20versusRestIncomeTrends20180602",region,".xlsx")
  P20main3=subset(P20main2, RegionCID==region)
  xl.workbook.add()
  countries=unique(P20main3$CountryName)
  for(country in countries){
    new=subset(P20main3, CountryName==country)
    max=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("CountryName","NP20average","Nrestaverage","Year")]
    names(new)=c("CountryName","National P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("CountryName","Year"))
    title=paste(country, "\n",Consumption,"Trends")
    # p=ggplot(data=new, aes(x=Year, y=Nrestaverage))+ geom_line(aes(y=NP20average, colour="National P20"))+ geom_line(aes(y=Nrestaverage, colour="Rest"))+labs(title=title,y=paste("Average daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year")+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+scale_y_continuous(labels=dollar)
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,max),labels=dollar)
    # +coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.sheet.delete("Sheet1")
  xl.workbook.save(filenam)
  xl.workbook.close()
}



##Regions
#Wealth Graph for App
load("C:/Users/Zach/Documents/Poverty data/AGGPovcalScrape1May2018.RData")

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


P20main= data.frame(regional.p20)
P20main$Populations = gsub(",","",P20main$Populations)
P20main$pop = as.numeric((P20main$Populations)) * 1000000
P20main$P20pop = (P20main$H/100)*(P20main$pop)
P20main$P20average = P20main$PovertyLine -((P20main$PovertyLine*(P20main$PG/100)*P20main$pop)/P20main$P20pop)
P20main$restpop = P20main$pop - P20main$P20pop
P20main$restaverage = (((P20main$Mean/(365/12))*P20main$pop)-(P20main$P20average * P20main$P20pop))/P20main$restpop

regions=unique(P20main$RegionCID)
countries=unique(P20main$RegionTitle)
names(P20main)[2]="Year"

for(region in regions){
  filenam=paste0("C:/Users/Zach/Documents/P20 Analysis 2018/Income comparisons P20 and rest/P20versusRestIncomeTrendsREG20180628",region,".xlsx")
   xl.workbook.add()
   message(region)
  for(country in countries){
    new=subset(P20main, RegionTitle==country)
    max=max(new$restaverage,new$Nrestaverage)+(.05*(max(new$restaverage,new$Nrestaverage)))
    Consumption=unique(new$Consumption)
    new=new[,c("RegionTitle","P20average","restaverage","Year")]
    names(new)=c("RegionTitle","P20","Rest of population","Year")
    new.m=melt(new,id.vars=c("RegionTitle","Year"))
    title=paste(country, "\n","Trends")
    p=ggplot(data=new.m, aes(x=Year, y=value,group=variable,colour=variable))+
      geom_line()+
      labs(title=title,y=paste("Ave. daily", str_to_lower(Consumption), "per person\nUSD 2011 PPP"),x="Year",colour="")+
      theme_classic()+
      theme(legend.title=element_blank(), plot.title=element_text(hjust=0.5))+
      expand_limits(y=0)+ 
      scale_y_continuous(expand=c(0,0),limits=c(0,max),labels=dollar)
    # coord_cartesian(ylim=c(0,max))
    print(p)
    y.plot=current.graphics()
    xl.sheet.add(substr(country,1,30))
    xl[a1]=y.plot
    xl[i1]=t(names(new))
    xl[i2]=new
    
  }
  xl.workbook.save(filenam)
  xl.workbook.close()
}
                              
  
  
  