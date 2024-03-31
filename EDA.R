#### load library ####

library(rhdfs)
hdfs.init()
library(rmr2)
library(ggplot2)
library(ggmap)
library(dplyr)
library(psych)
library(lubridate)
library(stringr)
library(corrplot)
library(dplyr)
library(RgoogleMaps)

#### full data load (for real analysis) ####

rmr.options(backend = "hadoop")
files <- hdfs.ls("/data/taxi/combined")$file
files
mr <- mapreduce(input = files[1],
                input.format =
                  make.input.format(format = "csv", sep=",",
                                    stringsAsFactors=F))

res <- from.dfs(mr)
ress <- values(res)

colnames.tmp <- as.character(ress[,1])
class.tmp <- as.character(ress[,2])
colnames <- colnames.tmp[-1]
class <- class.tmp[-1]
class[c(6,8,9,10)] <- "numeric"

input.format <- make.input.format(
  format = "csv", sep = ",",
  stringsAsFactors = F,
  col.names = colnames, colClasses = class)

files <- files[-1]

data <- mapreduce(input = files,
                  input.format = input.format)

res.data <- from.dfs(data)

taxi <- res.data$val

# medallion, payment_type, fare_amount, surcharge, mta_tax, tolls_amount, store_and_fwd_flag 변수 삭제
dat <- taxi[, c(-1, -2, -4, -5, -6, -7, -9, -12)] # 7920157 obs

str(dat)
# 현재 남아있는 변수 
# 13개의 변수 vendor_id, tip_amount, total_amount, rate_code
# pickup_datetime, dropoff_datetime, passenger_count, trip_time_in_secs
# trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude, drop_latitude


#### 결측치 제거 ####

# # 결측치 확인-> 184개의 NA 존재
# colSums(is.na(dat))  

# # 결측치가 존재하는 dropoff_latitude와 dropoff_longitude가 같은 행에 존재하는 것 확인 
# which(is.na(dat$dropoff_latitude)) == which(is.na(dat$dropoff_longitude))

# df <- filter(dat, is.na(dropoff_longitude))
# df$pickup_datetime == df$dropoff_datetime 

# # 그러면 이런 행들을 분석해보자
# df$vendor_id != 'CMT' # 다 CMT 회사인 것도 이상해

# df[df$pickup_datetime != df$dropoff_datetime,]

# df$tolls_amount == 0 

# 188개의 NA 제거 후 7,919,973 Obs.
dat <- na.omit(dat)

#### EDA ####
taxi_cor <- cor(dat[, c(-1, -2, -5, -6, -7)])
taxi_cor <- round(taxi_cor, 3)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(taxi_cor,
         method = "color",
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F
)

# vendor_id
table(dat$vendor_id) # CMT : 4335005, VTS : 4323795
plot(dat$vendor_id, main ='vendor_id variable')

# fare_amount
describe(dat$fare_amount)
plot(dat$fare_amount, main ='fare_amount variable')
# 186개의 음수값 존재 -> 조치 필요 (평균 대체, 제거, 등) 
sum( dat$fare_amount < 0 ) 

# surcharge
plot(dat$surcharge, main ='surcharge variable')

# mta_tax
plot(dat$mta_tax, main ='mta_tax variable')

# tips_amount
plot(dat$tips_amount, main ='tips_amount variable')

# tolls_amount
plot(dat$tolls_amount, main ='tolls_amount variable')

# total_amount
plot(dat$total_amount, main ='total_amount variable')

# rate_code
plot(dat$rate_code, main ='rate_code variable')

# pickup_datetime, dropoff_datetime -> pickup_time_bin과 pickup_day로 변경

# trim_time_in_secs

# trip_distance
plot(dat$trip_distance, main = 'trip_distance variable')
plot(dat$trip_distance, dat$total_amount)
plot(dat$trip_time_in_secs, dat$total_amount)

# pickup_longitude

# pickup_latitude
