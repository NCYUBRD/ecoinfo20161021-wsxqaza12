setwd("~/Desktop/R/raw")
library(data.table)

c0m530 <- fread("C0M530.txt",
                 na.strings = c('-9991','-9995','-9996','-9997','-9998','-9999'))

colnamesa <- c("ston","yyyymmddhh","PS01",'TX01','RH01','WD01','WD02','PP01','SS01')
setnames(c0m530, colnamesa)

c0m530[, timestamp:=  as.POSIXct(strptime(yyyymmddhh-1,'%Y%m%d%H'))]


## 資料分組(分為年及月及日)
c0m530 [, year:= substr ((c0m530$yyyymmddhh),3 ,4)]
c0m530 [, month:= substr ((c0m530$yyyymmddhh),5 ,6)]
c0m530 [, day:= substr ((c0m530$yyyymmddhh),7 ,8)]


# 十年平均
mean(c0m530$TX01,na.rm = T)

# 查詢某年或某月或某日之平均 x=2006/ 01/ 01
day_tem <- function(x){
  return(mean(c0m530$TX01[grepl(x, c0m530$yyyymmddhh)], na.rm = T))
}
  

# 使用aggreate 將分組好的group去計算十年內每月均溫
month_tem_mean <- aggregate(c0m530$TX01 ~ c0m530$month, c0m530, mean, na.rm = T)
# 降水量
month_pre_mean_front <- aggregate(c0m530$WD02 ~ c0m530$year + c0m530$month,
                            c0m530, mean, na.rm = T)
p <- c("year", "month", "pre")
setnames(month_pre_mean, p)
month_pre_mean <- aggregate(month_pre_mean_front$pre ~ month_pre_mean_front$month, 
                            month_pre_mean, na.rm = T)


### 計算一年中每天的溫度
day_tem_mean <- aggregate(c0m530$TX01 ~ c0m530$month + c0m530$day, c0m530, mean, na.rm = T)

# 取出每一天的最大與最小值
day_tem_min <- aggregate(c0m530$TX01 ~ (c0m530$year+ c0m530$month + c0m530$day), 
                         c0m530, min, na.rm = T)

day_tem_max <- aggregate(c0m530$TX01 ~ (c0m530$year+ c0m530$month + c0m530$day), 
                         c0m530, max, na.rm = T)


    ####day_tem_min[, day:= substr(c0m530$yyyymmddhh, 5, 8)]

# 計算每天之最暖及最冷(24小時內啥時最冷、最暖)
day_tem_min_mean <- aggregate(day_tem_min$`c0m530$TX01` ~ 
                                 day_tem_min$`c0m530$month` + day_tem_min$`c0m530$day`, 
                               day_tem_min, mean, na.rm = T)

day_tem_max_mean <- aggregate(day_tem_max$`c0m530$TX01` ~ 
                                 day_tem_max$`c0m530$month` + day_tem_max$`c0m530$day`,
                              day_tem_max, mean, na.rm = T)

#### 跑aggregate來看每年中最暖月及最冷越是誰
who_warm_front <- aggregate(c0m530$TX01 ~ c0m530$year + c0m530$month,
                            c0m530, mean, na.rm = T)

## 在who_waem_front中新增一欄年加月
i <- 1
while (i <= dim(who_warm_front)[1]) {
  who_warm_front[i, 4] <- paste(who_warm_front[i, 1], who_warm_front[i, 2], sep = "")
  i <- i+1
}
 
who_warm <- aggregate(who_warm_front$`c0m530$TX01` ~ who_warm_front$`c0m530$year`, 
                      who_warm_front, max, na.rm = T)

#  將who_warm中的欄位名稱替換，因為不知名的錯誤
a <- c("year", "tem_mean")
setnames(who_warm, a)

#  對比出到底那一年中最暖月是哪一個月份
i <- 1
while (i <= dim(who_warm)[1]) {
  which <- grepl(who_warm$tem[i], who_warm_front$`c0m530$TX01`)
  who_warm[i, 3] <- who_warm_front$V4[which]
  i <- i+1
}

# 最冷月也重複做一樣的事 這邊利用who_warm_front得值，因為已經將某年某月的平均算出來了
# ----------------------------------------------------------------------
who_cold <- aggregate(who_warm_front$`c0m530$TX01` ~ who_warm_front$`c0m530$year`, 
                      who_warm_front, min, na.rm = T)

#  將who_cold中的欄位名稱替換，因為不知名的錯誤
a <- c("year", "tem_mean")
setnames(who_cold, a)

#  對比出到底那一年中最冷月是哪一個月份
i <- 1
while (i <= dim(who_cold)[1]) {
  which <- grepl(who_cold$tem[i], who_warm_front$`c0m530$TX01`)
  who_cold[i, 3] <- who_warm_front$V4[which]
  i <- i+1
}

# ----------------------------------------------------------------------

### 計算最暖⽉的每⽇最⾼溫平均 用day_tem_max已算好的某年某月某日最暖時間
month_tem_max <- aggregate(day_tem_max$`c0m530$TX01` ~ 
                             day_tem_max$`c0m530$year` + day_tem_max$`c0m530$month`,
                           day_tem_max, mean)

a <- c("year", "month", "max")
setnames(month_tem_max, a)

#  新增一欄年加月方便索引資料
i <- 1
while (i <= dim(month_tem_max)[1]) {
  month_tem_max[i, 4] <- paste(month_tem_max[i, 1], month_tem_max[i, 2], sep = "")
  i <- i+1
}

# 在抓取who_warm中算出的該年最暖月份，並新增至第四欄中
i <- 1
while (i <= dim(who_warm)[1]) {
  which <- grepl(who_warm[i, 3], month_tem_max$V4)
  who_warm[i, 4] <- month_tem_max$max[which]
  i <- i+1
}

# 最冷月也做一樣的事
# ----------------------------------------------------------------------
### 計算最冷⽉的每⽇最⾼溫平均 用day_tem_min已算好的某年某月某日最暖時間
month_tem_min <- aggregate(day_tem_min$`c0m530$TX01` ~ 
                             day_tem_min$`c0m530$year` + day_tem_min$`c0m530$month`,
                           day_tem_min, mean)

a <- c("year", "month", "min")
setnames(month_tem_min, a)

#  新增一欄年加月方便索引資料
i <- 1
while (i <= dim(month_tem_min)[1]) {
  month_tem_min[i, 4] <- paste(month_tem_min[i, 1], month_tem_min[i, 2], sep = "")
  i <- i+1
}

# 在抓取who_cold中算出的該年最暖月份，並新增至第四欄中
i <- 1
while (i <= dim(who_cold)[1]) {
  which <- grepl(who_cold[i, 3], month_tem_min$V4)
  who_cold[i, 4] <- month_tem_min$min[which]
  i <- i+1
}

# ----------------------------------------------------------------------
cat("每⽇平均氣溫:day_tem_mean
    每⽇最低溫的平均:day_tem_min_mean
    每⽇最⾼溫的平均:day_tem_max_mean
    每⽉平均氣溫:month_tem_mean
    平均每⽉累積降⽔:month_pre_mean"
    )

x <- readline("請輸入今天的日期：")
