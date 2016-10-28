setwd("~/Desktop/R/raw")
library(data.table)

c0m5302 <- fread("C0M530.txt",
                 na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))

colnamesa <- c("ston","yyyymmddhh","PS01",'TX01','RH01','WD01','WD02','PP01','SS01')
setnames(c0m5302, colnamesa)

c0m5302[, timestamp:=  as.POSIXct(strptime(yyyymmddhh-1,'%Y%m%d%H'))]

# 十年平均
mean(c0m5302$TX01,na.rm = T)

# 每日平均
day_tem <- function(x){
  return(mean(c0m5302$TX01[grepl(x, c0m5302$yyyymmddhh)], na.rm = T))
}
  
#每月平均
  

