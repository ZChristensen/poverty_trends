list.of.packages <- c("RPostgreSQL","data.table","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


conf=fromJSON("E:/git/povcal-sql/config.json")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv
                 ,dbname="povcal"
                 ,host = "localhost"
                 ,port = 5432
                 ,user = "postgres"
                 ,password = conf$password
)
smy_190 = dbGetQuery(con, 'select * from "PovCalNetSmy" where "PovertyLine"=1.90')
dbGetQuery(con, 'select distinct "RequestYear" from "PovCalNetSmy"')


agg = dbReadTable(con, "PovCalNetAgg")
save(smy,file="E:/git/poverty_trends/data/SMYPovcalScrape3April2019.RData")
save(agg,file="E:/git/poverty_trends/data/AGGPovcalScrape3April2019.RData")

