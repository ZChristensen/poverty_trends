
list.of.packages <- c("data.table","readr","Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd="E:/git/poverty_trends"
setwd(wd)

