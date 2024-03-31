#### 파생변수 생성 : pickup_day, dropoff_day, pickup_time_bin, dropoff_time_bin ####

## [요일 변수 생성 : pickup_day, dropoff_day ] ##
pickup.date <- as.Date(dat$pickup_datetime)
wday(pickup.date)
dat$pickup_day <- wday(pickup.date, label = TRUE)

dropoff.date <- as.Date(dat$dropoff_datetime)
wday(dropoff.date)
dat$dropoff_day <- wday(dropoff.date, label = TRUE)

summary(dat$pickup_day)
summary(dat$dropoff_day)

# pickup_day
plot(dat$pickup_day, main ='pickup_day variable')

# dropoff_day
plot(dat$dropoff_day, main ='dropoff_day variable')


## [시간 변수 생성 : pickup_time_bin, dropoff_time_bin] ##

# pickup1: 00시-04시 / pickup2: 04-08시 / pickup3: 08-12시 / pickup4: 12-16시 / pickup5: 16-20시 / pickup6: 20-24시

bin_fun <- function(x) {
  d1 <- x
  d1 <- str_replace_all(d1, "\\-", "")
  d1 <- str_replace_all(d1, "\\:", "")
  d1 <- as.numeric(str_sub(d1, 10, 16))
  y <- cut( x = d1, breaks = c(0, 40000, 80000, 120000, 160000, 200000, 235960), 
            right = F, labels = c('pickup1', 'pickup2', 'pickup3', 'pickup4', 'pickup5', 'pickup6'))
}

dat$pickup_time_bin <- bin_fun(dat$pickup_datetime)
dat$dropoff_time_bin <- bin_fun(dat$dropoff_datetime)

summary(dat$pickup_time_bin)
summary(dat$dropoff_time_bin)

# pickup_time_bin
plot(dat$pickup_time_bin, main ='pickup_time_bin variable')

# dropoff_time_bin
plot(dat$dropoff_time_bin, main ='dropoff_time_bin variable')

## 기존변수 제거 ##
dat <- subset(dat, select=-c(pickup_datetime, dropoff_datetime))

#### Drop outlier ####
str(dat) # 7919973 obs. of  15 variables

## tips_amount ##
describe(dat$tip_amount)
plot(dat$tip_amount)
boxplot(dat$tip_amount)

# tip_amount < 0  제거
dat <- dat[-which(dat$tip_amount < 0), ]

plot(dat$tip_amount, main = 'tip_amount variable')
boxplot(dat$tip_amount)

## trip_distance ##
describe(dat$trip_distance)
plot(dat$trip_distance, main = 'trip_distance before outlier removal')

## 뉴욕주 최장거리로 횡단 시 60 mile, 100 mile 이상의 값 삭제
dat <- dat[-which(dat$trip_distance >= 100),]
summary(dat$trip_distance)

plot(dat$trip_distance, main = 'trip_distance after outlier removal')

## passenger_count ## 
summary(dat$passenger_count)
describe(dat$passenger_count)
plot(dat$passenger_count)
table(dat$passenger_count)

# passenger_count == 0  | passenger_count >= 7 제거
dat <- dat[-which(dat$passenger_count == 0 | dat$passenger_count >= 7),]

# passenger_count 변수 제거
dat <- subset(dat, select=-passenger_count)
str(dat) # 7919831 Obs. of 14 variable

# total_amount 전처리
total_amount_max <- max(dat$total_amount)
dat <- dat[-which(dat$total_amount <= 0 | dat$total_amount == total_amount_max), ]

# rate_code 전처리 
dat <- dat[which(dat$rate_code==1),]

# trip_time_in_secs 전처리
dat <- dat[-which(dat$trip_time_in_secs >= 4000000), ]

## 뉴욕주 경도 범위 -74.2 ~ -73.5
## 뉴욕주 위도 범위 40.45 ~ 40.95
## 뉴욕주 위도 경도에서 벗어나는 점들 제외
# longitude, latitude 전처리
dat <- dat[-which(dat$pickup_longitude <= -74.2
                  | dat$pickup_longitude >= -73.5) ,]
dat <- dat[-which(dat$pickup_latitude <= 40.45 
                  | dat$pickup_latitude >= 40.95) ,]
dat <- dat[-which(dat$dropoff_longitude <= -74.2 
                  | dat$dropoff_longitude >= -73.5) ,]
dat <- dat[-which(dat$dropoff_latitude <= 40.45 
                  | dat$dropoff_latitude >= 40.95) ,]
