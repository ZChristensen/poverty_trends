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
smy = dbReadTable(con, "PovCalNetSmy")
svy = dbReadTable(con, "PovCalNetSvy")
save(smy,file="E:/git/poverty_trends/data/SMYPovcalScrape6April2019.RData")
save(agg,file="E:/git/poverty_trends/data/AGGPovcalScrape6April2019.RData")
save(svy,file="E:/git/poverty_trends/data/SVYPovcalScrape6April2019.RData")
