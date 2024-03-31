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

#### 회귀식 추정 ####

map.fun <- function(k, v){
  dat <- data.frame(total = v$total_amount, dist = v$trip_distance, 
                    time = v$trip_time_in_secs, tip = v$tip_amount
  )
  Xk <- model.matrix(total ~ ., dat)
  yk <- as.matrix(dat[,1])
  XtXk <- crossprod(Xk,Xk)
  Xtyk <- crossprod(Xk,yk)
  ytyk <- crossprod(yk,yk)
  res<-list(XtXk, Xtyk, ytyk)
  keyval(1, res)
}

reduce.fun <- function(k, v) {
  XtX <- Reduce("+",v[seq_along(v) %% 3 == 1])
  Xty <- Reduce("+",v[seq_along(v) %% 3 == 2])
  yty <- Reduce("+",v[seq_along(v) %% 3 == 0])
  
  res <- list(XtX = XtX, Xty = Xty, yty = yty)
  keyval(1, res)
}

# summary : beta.hat과 MSE등 분석 결과
fun.summary <- function(v) {
  XtX = v$XtX
  Xty = v$Xty
  yty = v$yty
  beta.hat = solve(XtX, Xty)
  nn = XtX[1,1]
  ysum = Xty[1]
  ybar = ysum/nn
  stat <- list(nn = nn, beta.hat = beta.hat,
               ysum = ysum, ybar = ybar)
  SSE = yty - crossprod(beta.hat, Xty)
  SST = yty - ysum^2/nn
  SSR = SST - SSE
  R2 = SSR/SST
  SS <- list(SSR = SSR, SSE = SSE, SST =
               SST, R2 = R2)
  df.reg = dim(XtX)[1L] - 1
  df.tot = nn - 1
  df.res = df.tot - df.reg
  DF <- list(df.reg = df.reg, df.res = df.res,
             df.tot = df.tot)
  MSR = SSR / df.reg
  MST = SST / df.tot
  MSE = SSE / df.res
  MS <- list(MSR = MSR, MSE = MSE, MST
             = MST)
  f.val = MS$MSR / MS$MSE
  p.val = pf(f.val, DF$df.reg, DF$df.res,
             lower.tail = F)
  anova <- list(DF = DF, SS = SS, MS =
                  MS, f.val = f.val, p.val = p.val)
  res <- list(mat = v, stat = stat, anova =
                anova)
}

### regression function ###
reg.fun <- function(arr) {
  reg_dat <- arr[, c(3, 2, 5, 6)]
  reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg_dat),
                                          map=map.fun, 
                                          reduce=reduce.fun, combine = T)))
  reg.summary <- fun.summary(reg.result)
  reg.summary$stat
  reg.summary$anova
  
  # 잔차가 큰 값 선택, Design Matrix
  taxiDesingMat <- model.matrix(object =
                                  total_amount ~ trip_distance +
                                  trip_time_in_secs + tip_amount, data =
                                  reg_dat[,c("trip_distance", "trip_time_in_secs", "tip_amount",
                                             "total_amount")])
  # 추정치 계산
  fittedFare <- t(reg.summary$stat$beta.hat) %*% t(taxiDesingMat)
  fittedFare <- t(fittedFare)
  resi <- reg_dat$total_amount - fittedFare[,1]
  
  # 잔차
  BigRes <- sort(resi, decreasing = T) %>%
    names() # 잔차 INDEX
  BigResIDX <-
    BigRes[1:ceiling(length(BigRes)*0.001)] # 잔차 INDEX 중 상위 0.1%
  
  # 실제 낸 돈 - 추정한 정상금액 == 잔차
  reg_dat[BigResIDX,] %>%
    mutate(estimateFare =
             fittedFare[,1][BigResIDX], residual =
             resi[BigResIDX]) %>% select(total_amount,
                                         estimateFare, residual) %>% head()
  reg_dat[BigResIDX,] %>% mutate(estimateFare =
                                   fittedFare[,1][BigResIDX], residual =
                                   resi[BigResIDX]) %>% select(total_amount,
                                                               estimateFare, residual) %>% tail()
  
  # 변수가 다 있는 데이터로 돌아옴
  res <- dat[BigResIDX, ]
  return (res)
}

selectedDataG1 <- reg.fun(picktime1_dat)
str(selectedDataG1)
selectedDataG2 <- reg.fun(picktime2_dat)
str(selectedDataG2)
selectedDataG3 <- reg.fun(picktime3_dat)
str(selectedDataG3)
selectedDataG4 <- reg.fun(picktime4_dat)
str(selectedDataG4)
selectedDataG5 <- reg.fun(picktime5_dat)
str(selectedDataG5)
selectedDataG6 <- reg.fun(picktime6_dat)
str(selectedDataG6)

#### mapping ####

map.center.loc <- c(40.6643, -73.938)
input_zoom <- 11

NewYork_hotplace <- data.frame(latitude = c(40.755, 40.741, 40.753, 40.748, 40.759, 40.779, 
                                            40.762, 40.689, 40.709, 40.753 ), 
                               longitude = c(-73.987, -73.990, -73.982, -73.986, -73.981, -73.963, 
                                             -73.977, -74.045, -74.000, -73.977))

NewYork_hotelplace <- data.frame(latitude = c(40.747, 40.718, 40.743, 40.766, 40.765,
                                              40.7111, 40.614, 40.640, 40.711, 40.801),
                                 longitude = c(-74.025, -74.047, -73.902, -73.978, -73.930,
                                               -74.066, -74.002, -74.282, -73.807, -74.011))

#### pick mapping ####

maps.pickinfo <- selectedDataG1[, c(8, 7)]
maps.dropinfo <- selectedDataG1[, c(10, 9)]

map <- GetMap(center = map.center.loc,
              zoom = input_zoom, maptype = "roadmap", format = 'png8',
              destfile= "map.png")

PlotOnStaticMap(map, lat = maps.pickinfo$pickup_latitude, lon = maps.pickinfo$pickup_longitude,
                destfile = 'map.point.png', cex = 0.5, pch = 20, col = 'purple', add= FALSE)

PlotOnStaticMap(map, lat = NewYork_hotplace$latitude, lon = NewYork_hotplace$longitude,
                destfile = 'map.point.png', cex = 3, pch = '*', col = 'red', add= TRUE)

PlotOnStaticMap(map, lat = NewYork_hotelplace$latitude, lon = NewYork_hotelplace$longitude,
                destfile = 'map.point.png', cex = 3, pch = '*', col = 'black', add= TRUE)

legend("bottomright",  
       legend=c("pickup", "hot place", "hotel place"),   
       col=c("purple", "red", "black"), lwd=3)  

#### drop mapping ####

#map <- GetMap(center = map.center.loc,
zoom = input_zoom, maptype = "roadmap", format = 'png8',
destfile= "map.png")

#PlotOnStaticMap(map, lat = maps.dropinfo$dropoff_latitude, lon = maps.dropinfo$dropoff_longitude,
destfile = 'map.point.png', cex = 0.5, pch = 20, col = 'purple', add= FALSE)

#PlotOnStaticMap(map, lat = NewYork_hotplace$latitude, lon = NewYork_hotplace$longitude,
destfile = 'map.point.png', cex = 3, pch = '*', col = 'red', add= TRUE)

PlotOnStaticMap(map, lat = NewYork_hotelplace$latitude, lon = NewYork_hotelplace$longitude,
                destfile = 'map.point.png', cex = 3, pch = '*', col = 'black', add= TRUE)

legend("bottomright",  
       legend=c("dropoff", "hot place", "hotel place"),   
       col=c("purple", "red", "black"), lwd=3)  

ggplot(data = selectedDataG1, aes(x=pickup_day, fill=pickup_day))+
  geom_bar(position="dodge")+
  ggtitle("Plot of vendor_id by pickup_day\n")+
  theme(plot.title = element_text(hjust = 0.5))


# 전체 0.1% 상위 택시 기사 데이터 
selectedData <- rbind(selectedDataG1,selectedDataG2,selectedDataG3,selectedDataG4,selectedDataG5,selectedDataG6)

ggplot(selectedData, aes(x= factor(pickup_day), fill = pickup_day)) +
  geom_bar() + 
  ggtitle("Plot of Frequency in taxi by pickup_day\n")

ggplot(selectedData, aes(x= factor(dropoff_day), fill = dropoff_day)) +
  geom_bar() + 
  ggtitle("Plot of Frequency in taxi by dropoff_day\n")
