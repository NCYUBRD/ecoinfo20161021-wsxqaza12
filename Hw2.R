setwd("~/Desktop/R/raw")
library(data.table)

c0m530 <- fread("C0M530.txt",
                 na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))

colnamesa <- c("ston","yyyymmddhh","PS01",'TX01','RH01','WD01','WD02','PP01','SS01')
setnames(c0m530, colnamesa)

c0m530[, timestamp:=  as.POSIXct(strptime(yyyymmddhh-1,'%Y%m%d%H'))]


## 資料分組(分為年及月及日)
c0m530 [, month:= substr ((c0m530$yyyymmddhh),5 ,6)]
c0m530 [, day:= substr ((c0m530$yyyymmddhh),5 ,8)]
c0m530 [, year:= substr ((c0m530$yyyymmddhh),3 ,8)]

# 十年平均
mean(c0m530$TX01,na.rm = T)

# 某年或某月或某日平均 x=2006/ 01/ 01
day_tem <- function(x){
  return(mean(c0m530$TX01[grepl(x, c0m530$yyyymmddhh)], na.rm = T))
}
  

# 使用aggreate 將分組好的group去計算十年內每月均溫
month_tem_mean <- aggregate(c0m530$TX01 ~ c0m530$month, c0m530, mean, na.rm = T)
# 降水量
month_pre_mean <- aggregate(c0m530$WD02 ~ c0m530$month, c0m530, mean, na.rm = T)

# 計算一年中每天的溫度
day_tem_mean <- aggregate(c0m530$TX01 ~ c0m530$day, c0m530, mean, na.rm = T)
# 取出每一天的最大與最小值
day_tem_min <- aggregate(c0m530$TX01 ~ c0m530$year+ c0m530$day, c0m530, min, na.rm = T)
day_tem_max <- aggregate(c0m530$TX01 ~ c0m530$year+ c0m530$day, c0m530, max, na.rm = T)


    ####day_tem_min[, day:= substr(c0m530$yyyymmddhh, 5, 8)]

# 計算每天之最暖及最冷(24小時內啥時最冷、最暖)
day_tem_min_mean <- aggregate(day_tem_min$`c0m530$TX01` ~ day_tem_min$`c0m530$day`, 
                              day_tem_min, mean, na.rm = T)
day_tem_max_mean <- aggregate(day_tem_max$`c0m530$TX01` ~ day_tem_max$`c0m530$day`,
                              day_tem_max, mean, na.rm = T)

# 在c0m530中再加一欄年跟月的分組，跑aggregate來看每年中最暖月及最冷越是誰
c0m530 [, mon_year:= substr ((c0m530$yyyymmddhh),3 ,6)]
who_warm <- aggregate(c0m530$TX01 ~ c0m530$mon_year, c0m530, mean, na.rm = T)


