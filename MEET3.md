# MEET 3

날짜: 2022년 11월 28일 오후 9:00 (GMT+9)
분류: 정기회의
여부: 예정
참여자: 천진영, ***, ***, ***

## **[ MEET2 회의 내용 ]**

### 사용할 변수

- 분석에 사용
    - hack_license
    - Vender ID : A code indicating the TPEP provider that provided the record. →
    - pickup_time_bin → 4시간 기준으로 범주화
    - dropoff_time_bin → 4시간 기준으로 범주화
    - Passenger count : The number of passengers in the vehicle. This is a driver-entered value. → 차량의 승객 수를 의미
        - 새벽에 돌려보고 결과 알려주기
    - Trip distance : The elapsed trip distance in miles reported by the taximeter. → 택시미터에 의한 주행 거리(마일기준)를 의미
    - RateCodeID : The final rate code in effect at the end of the trip. → 여행이 끝날 때 적용되는 최종 요금 코드 → **각 값들은 출발지를 의미 ;1 = standard rate**
    - Payment type : A numeric code signifying how the passenger paid for the trip. → 지불방식을 의미
        - 1 = credit card
        - 2 = cash
        - 3 = no charge
        - 4 = dispute (무슨 뜻이지?)
        - 5 = unknown
        - 6 = voided trip (무슨 뜻이지?)
    - Tip amount : Tip amount – This field is automatically populated for credit card tips. Cash tips are not included. → 팁 금액 (신용 카드 팁에 자동으로 입력, 현금 팁은 포함X)
    - Total amount : The total amount charged to passengers. Does not include cash tips. → 승객에게 청구된 총 금액 (현금 팁 포함 X)
- mapping에 사용
    - Pickup longitude : Longitude where the meter was engaged. → 미터기가 활성화된 경도를 의미
    - Pickup latitude : Latitude where the meter was engaged. → 미터기가 활성화된 위도를 의미
    - Dropoff longitude : Longitude where the meter was disengaged. → 미터기가 해제된 경도를 의미
    - Dropoff latitude : Latitude where the meter was disengaged. → 미터기가 해제된 위도를 의미
    

### 전처리 방향

- hack_license → 전처리 X
- vender_id → 전처리 X
    
    # CMT : 4335005, VTS : 4323795
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled.png)
    
- tip_amount → 확인해보기  @천진영
    
    
- total_amount → 0보다 작은 값 제거, max값 제거 @*** @***
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%201.png)
    
    대부분이 0-200$ 사이의 값을 갖고 있으며 가장 값이 큰 1000$ 근접한 자료도 살펴볼 필요 있음. **0보다 작은 total_amount는 제거**
    
- rate_code == 1인 값만 사용, 나머지 값은 제거@*** @***
    
    ![rate_code.jpg](MEET%203%200bc772662eb64dc39266ba15785535c3/rate_code.jpg)
    
    rate_code == 1인 standard rate의 비율이 가장 많음. 1 - 6 값 의외에 이상치들도 나타남 → **제거 필요**
    
- trip_time_in_secs > 4000000 인 값들 제거하기 (제거하고 분석에는 사용 X - 이상치 제거에만 사용) @*** @***
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%202.png)
    
- trip_distance → 확인해보기 @천진영
- pickup_day → 전처리 X
    
    ![pickup_day.jpg](MEET%203%200bc772662eb64dc39266ba15785535c3/pickup_day.jpg)
    
    월요일에 가장 pickup이 가장 적은 것을 확인할 수 있고, 목, 금, 토요일이 많음.
    
- dropoff_day → 전처리 X
    
    ![dropoff_day.jpg](MEET%203%200bc772662eb64dc39266ba15785535c3/dropoff_day.jpg)
    
    pickup과 동일한 분포
    
- ~~pickup_time_bin → 4시간 기준으로 범주화 @천진영~~
    
    ![pickup_time_bin.jpg](MEET%203%200bc772662eb64dc39266ba15785535c3/pickup_time_bin.jpg)
    
    밤 시간대 21:00 ~ 23:59 가장 택시 pickup이 많고 새벽이 가장 적음.
    
- ~~dropoff_time_bin → 4시간 기준으로 범주화  @천진영~~
    
    ![dropoff_time_bin.jpg](MEET%203%200bc772662eb64dc39266ba15785535c3/dropoff_time_bin.jpg)
    
    pickup과 같은 분포
    
- passenger_count → 확인해보고 알려주기 @천진영

변수선택법 고민해보기 @천진영 

- 프로젝트 간략 요약 (1page) @***
- 프로젝트 선정 배경 및 목적 (1page) @***
- 본론 - 프로젝트 수행 과정 (max 5page)
    - 데이터 정제(전처리) 과정 포함
- 결과 (max 4page)
    - 중요 그림, 테이블 포함
- 후기 (max 1page)
    - 프로젝트를 통해 배운점
    - 프로젝트 수행 시 각 팀원의 역할, ~

- 서론 작성 @***
    - hack_license → 운전자의 면허를 의미하는 변수 : 해당 변수를 사용해서 total_amount가 많은 (taxi 승객을 많이 태운 기사들)의 특징을 뽑아서 가설을 세운다
    - ex) 택시를 많이 태운 사람들은 어느 시간대 어느 위치, 어느 요일 뭐 이런거 정도 ? plot 정도 찍어보고 보고서에 말로 표현하기

---

## **[ 분석 내용 ]**

## 진영

### Load library

```r
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
```

- 

### Load 1 file data (for analysis)

```r
# 1 file data load 

rmr.options(backend = "hadoop")
files <- hdfs.ls("/data/taxi/combined")$file
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

data <- mapreduce(input = files[1],
                  input.format = input.format)

res.data <- from.dfs(data)

taxi <- res.data$val

# medallion, payment_type, fare_amount, surcharge, mta_tax, tolls_amount, store_and_fwd_flag 변수 삭제
dat <- taxi[, c(-1, -4, -5, -6, -7, -9, -12)] 
# 현재 남아있는 변수 
# 14개의 변수 hack_license, vendor_id, tip_amount, total_amount, rate_code
# pickup_datetime, dropoff_datetime, passenger_count, trip_time_in_secs
# trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude, drop_latitude
```

- 

### Load full data (for real analysis)

```r
# full data load 

rmr.options(backend = "hadoop")
files <- hdfs.ls("/data/taxi/combined")$file
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
dat <- taxi[, c(-1, -4, -5, -6, -7, -9, -12)] 
# 현재 남아있는 변수 
# 14개의 변수 hack_license, vendor_id, tip_amount, total_amount, rate_code
# pickup_datetime, dropoff_datetime, passenger_count, trip_time_in_secs
# trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude, drop_latitude
```

### EDA

```r
# 결측치 확인-> 188개의 NA 존재
colSums(is.na(dat))  

# 결측치가 존재하는 dropoff_latitude와 dropoff_longitude가 같은 행에 존재하는 것 확인 
which(is.na(dat$dropoff_latitude)) == which(is.na(dat$dropoff_longitude))

df <- filter(dat, is.na(dropoff_longitude))
df$pickup_datetime == df$dropoff_datetime 

# 그러면 이런 행들을 분석해보자
df$vendor_id != 'CMT' # 다 CMT 회사인 것도 이상해

df[df$pickup_datetime != df$dropoff_datetime,]

df$tolls_amount == 0 

# 188개의 NA 제거 후 8,658,988 Obs.
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

# trim_time_in_secs -> 이상치 제거에 사용
plot(dat$trim_time_in_secs, main = 'trim_time_in_secs')

# trip_distance
plot(dat$trip_distance, main = 'trip_distance variable')
plot(dat$trip_distance, dat$total_amount)
plot(dat$trip_time_in_secs, dat$total_amount)

# pickup_day
plot(dat$pickup_day, main ='pickup_day variable')

# dropoff_day
plot(dat$dropoff_day, main ='dropoff_day variable')

# pickup_time_bin
plot(dat$pickup_time_bin, main ='pickup_time_bin variable')

# dropoff_time_bin
plot(dat$dropoff_time_bin, main ='dropoff_time_bin variable')
```

### Derived variable

```r
#### 파생변수 생성 ####

## [요일 변수 생성 : pickup_day, dropoff_day ] ##
pickup.date <- as.Date(dat$pickup_datetime)
wday(pickup.date)
dat$pickup_day <- wday(pickup.date, label = TRUE)

dropoff.date <- as.Date(dat$dropoff_datetime)
wday(dropoff.date)
*dat$dropoff_day <- wday(dropoff.date, label = TRUE)

summary(dat$pickup_day)
summary(dat$dropoff_day)

# pickup_day
plot(dat$pickup_day, main ='pickup_day variable')

# dropoff_day
plot(dat$dropoff_day, main ='dropoff_day variable')

## [시간 변수 생성 : pickup_time_bin, dropoff_time_bin] ##

bin_fun <- function(x) {
  d1 <- x
  d1 <- str_replace_all(d1, "\\-", "")
  d1 <- str_replace_all(d1, "\\:", "")
  d1 <- as.numeric(str*_sub(d1, 10, 16))
  y <- cut( x = d1, breaks = c(0, 40000, 80000, 1200000, 160000, 200000, 235960), 
            right = F, labels = c('00시-04시', '04시-08시', '08시-12시', '12시-16시', '16시-20시', '20시-24시'))
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
```

### Drop outlier

- tip_amount
    
    ![이상치 제거 전 - tip_amount plot  ](MEET%203%200bc772662eb64dc39266ba15785535c3/tip_amount.jpg)
    
    이상치 제거 전 - tip_amount plot  
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%203.png)
    
    ![이상치 제거 전 - tip_amount boxplot  ](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%204.png)
    
    이상치 제거 전 - tip_amount boxplot  
    
    ```r
    # 음수인 값만 제거해도 된다고 판단
    
    ## tips_amount ##
    head(dat$tip_amount)
    describe(dat$tip_amount)
    plot(dat$tip_amount, main = 'tip_amount : Before outlier removal')
    boxplot(dat$tip_amount)
    
    # tip_amount < 0  제거
    dat <- dat[-which(dat$tip_amount < 0), ]
    
    plot(dat$tip_amount, main = 'tip_amount : After outlier removal')
    ```
    
    ![이상치 제거 후 - tip_amount plot  ](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%205.png)
    
    이상치 제거 후 - tip_amount plot  
    
- trip_distance
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%206.png)
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%207.png)
    
    ```r
    ## trip_distance ##
    head(dat$trip_distance)
    describe(dat$trip_distance)
    plot(dat$trip_distance, main = 'trip_distance before outlier removal')
    
    ## 뉴욕주 최장거리로 횡단 시 60 mile, 100 mile 이상의 값 삭제
    dat <- dat[-which(dat$trip_distance >= 100),]
    summary(dat$trip_distance)
    
    plot(dat$trip_distance, main = 'trip_distance after outlier removal')
    ```
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%208.png)
    
- passenger_count
    
    ![passenger_count plot](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%209.png)
    
    passenger_count plot
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2010.png)
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2011.png)
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2012.png)
    
    ```r
    ## passenger_count ## 
    summary(dat$passenger_count)
    describe(dat$passenger_count)
    plot(dat$passenger_count)
    table(dat$passenger_count)
    
    # passenger_count == 0  | passenger_count >= 7 제거
    dat <- dat[-which(dat$passenger_count == 0 | dat$passenger_count >= 7),]
    
    # passenger_count 변수 제거
    dat <- subset(dat, select=-passenger_count)
    str(dat)
    ```
    
    passenger_count가 높을 수록 total_amount가 높지 않다. + passenger_count가 0이 되는 값도 말되안된다고 생각. passenger_count가 0명인 데이터와 표본이 적은 7, 8, 208명인 데이터는 이상치 제거에만 사용하고 분석에는 제외 
    
- hack_license
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2013.png)
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2014.png)
    
    ```r
    ## hack_license ##
    t1 <- dat$hack_license[which(duplicated(dat$hack_license))]
    t2 <- dat$total_amount[which(duplicated(dat$hack_license))]
    
    tmp1 <- dat %>%
                group_by(hack_license) %>%
                summarise(count = n(), mean(total_amount))
    str(tmp1)
    head(tmp1)
    
    plot(tmp1$count, tmp1$`mean(total_amount)`)
    
    # hack_license의 count가 많다고 해서 total_amount의 값들이 높은 것은 아님.
    # 여기서 count == 1인 값들은 중복이 1회이니 실제 dat에서는 2개의 값을 갖음.
    # 그렇다면 hack_license의 count가 많은 사람들은 어떤 특징을 갖고 있을까 ?
    ```
    

## 전처리팀

```
'data.frame':	7920043 obs. of  18 variables:
 $ vendor_id        : Factor w/ 2 levels "CMT","VTS": 1 1 2 2 1 1 1 1 1 2 ...
 $ fare_amount      : num  5.5 14 22 5 8.5 15.5 7 17.5 14 3.5 ...
 $ surcharge        : num  0.5 0.5 0.5 0 0.5 0.5 0 0.5 0.5 0 ...
 $ mta_tax          : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
 $ tip_amount       : num  0 0 4 1 1.9 0 0 0 0 0 ...
 $ tolls_amount     : num  0 0 0 0 0 0 0 0 0 0 ...
 $ total_amount     : num  6.5 15 27 6.5 11.4 16.5 7.5 18.5 15 4 ...
 $ rate_code        : Factor w/ 11 levels "0","1","2","3",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ pickup_datetime  : chr  "2013-06-25 01:18:18" "2013-06-26 20:48:10" "2013-06-22 20:08:00" "2013-06-19 06:54:00" ...
 $ dropoff_datetime : chr  "2013-06-25 01:21:35" "2013-06-26 21:05:41" "2013-06-22 20:25:00" "2013-06-19 06:57:00" ...
 $ trip_time_in_secs: int  196 1050 1020 180 598 826 487 1413 1115 120 ...
 $ trip_distance    : num  1.1 3.4 6.92 0.95 1.5 4.6 0.9 3.8 2.9 0.46 ...
 $ pickup_longitude : num  -74 -74 -74 -74 -74 ...
 $ pickup_latitude  : num  40.8 40.7 40.7 40.7 40.7 ...
 $ dropoff_longitude: num  -74 -74 -74 -74 -74 ...
 $ dropoff_latitude : num  40.8 40.8 40.8 40.8 40.7 ...
 $ pickup_time_bin  : Factor w/ 6 levels "00시-04시","04시-08시",..: 1 6 6 2 6 6 3 1 6 4 ...
 $ dropoff_time_bin : Factor w/ 6 levels "00시-04시","04시-08시",..: 1 6 6 2 6 6 3 1 6 4 ...
```

- 사용 변수 전처리 code
    
    ```r
    # total_amount 전처리
    total_amount_max <- max(dat$total_amount)
    dat <- dat[-which(dat$total_amount <= 0 | dat$total_amount == total_amount_max), ]
    
    # rate_code 전처리 
    dat <- dat[which(dat$rate_code==1),]
    
    # trip_time_in_secs 전처리
    dat <- dat[-which(dat$trip_time_in_secs >= 4000000), ]
    
    bin_fun <- function(x) {
      d1 <- x
      d1 <- str_replace_all(d1, "\\-", "")
      d1 <- str_replace_all(d1, "\\:", "")
      d1 <- as.numeric(str_sub(d1, 10, 16))
      y <- cut( x = d1, breaks = c(0, 40000, 80000, 120000, 160000, 200000, 235960), 
                right = F, labels = c('00시-04시', '04시-08시', '08시-12시', '12시-16시', '16시-20시', '20시-24시'))
    }
    
    #pickup_time_bin 범주화 
    dat$pickup_time_bin <- bin_fun(dat$pickup_datetime)
    #dropoff_time_bin 범주화
    dat$dropoff_time_bin <- bin_fun(dat$dropoff_datetime)
    ```
    
- total_amount 전처리 결과
    - 0 이하 값, 800 이상 값 제거
    
    ![total_amount 전처리 전](MEET%203%200bc772662eb64dc39266ba15785535c3/total_amount_%25EC%25A0%2584%25EC%25B2%2598%25EB%25A6%25AC%25EC%25A0%2584.png)
    
    total_amount 전처리 전
    
    ![total_amount 전처리 후](MEET%203%200bc772662eb64dc39266ba15785535c3/total_amount_%25EC%25A0%2584%25EC%25B2%2598%25EB%25A6%25AC_%25ED%259B%2584.png)
    
    total_amount 전처리 후
    
- rate_code 전처리 결과
    - rate_code 1 제외 값 제거
    
    ![ratr_code 1 만 남기기.PNG](MEET%203%200bc772662eb64dc39266ba15785535c3/ratr_code_1_%25EB%25A7%258C_%25EB%2582%25A8%25EA%25B8%25B0%25EA%25B8%25B0.png)
    
- trip_time_in_secs 전처리 결과
    - 이상값 제거
    
    ![trip_time_in_secs 전처리 전](MEET%203%200bc772662eb64dc39266ba15785535c3/trip_time_in_secs_%25EC%25A0%2584%25EC%25B2%2598%25EB%25A6%25AC%25EC%25A0%2584.png)
    
    trip_time_in_secs 전처리 전
    
    ![trip_time_in_secs 전처리 후](MEET%203%200bc772662eb64dc39266ba15785535c3/trip_time_in_secs_%25EC%25A0%2584%25EC%25B2%2598%25EB%25A6%25AC%25ED%259B%2584.png)
    
    trip_time_in_secs 전처리 후
    
- pickup_time_bin 4시간 단위로 범주화

![스크린샷 2022-11-28 01.04.44.png](MEET%203%200bc772662eb64dc39266ba15785535c3/%25E1%2584%2589%25E1%2585%25B3%25E1%2584%258F%25E1%2585%25B3%25E1%2584%2585%25E1%2585%25B5%25E1%2586%25AB%25E1%2584%2589%25E1%2585%25A3%25E1%2586%25BA_2022-11-28_01.04.44.png)

![스크린샷 2022-11-28 01.12.42.png](MEET%203%200bc772662eb64dc39266ba15785535c3/%25E1%2584%2589%25E1%2585%25B3%25E1%2584%258F%25E1%2585%25B3%25E1%2584%2585%25E1%2585%25B5%25E1%2586%25AB%25E1%2584%2589%25E1%2585%25A3%25E1%2586%25BA_2022-11-28_01.12.42.png)

- dropoff_time_bin 4시간 단위로 범주화
    
    ![스크린샷 2022-11-28 01.12.21.png](MEET%203%200bc772662eb64dc39266ba15785535c3/%25E1%2584%2589%25E1%2585%25B3%25E1%2584%258F%25E1%2585%25B3%25E1%2584%2585%25E1%2585%25B5%25E1%2586%25AB%25E1%2584%2589%25E1%2585%25A3%25E1%2586%25BA_2022-11-28_01.12.21.png)
    
    ![스크린샷 2022-11-28 01.12.51.png](MEET%203%200bc772662eb64dc39266ba15785535c3/%25E1%2584%2589%25E1%2585%25B3%25E1%2584%258F%25E1%2585%25B3%25E1%2584%2585%25E1%2585%25B5%25E1%2586%25AB%25E1%2584%2589%25E1%2585%25A3%25E1%2586%25BA_2022-11-28_01.12.51.png)
    
- 1차 데이터 정제 결과 총 18개 변수, 7,920,043개의 관측값 도출
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2015.png)
    

- fare_amount 186개 음수값 제거
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2016.png)
    
- 결측치 제거 확인
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2017.png)
    
- vendor_id 이상치 확인 - CMT, VTS 외 다른 값 없음
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2018.png)
    
- surcharge(통행료) - 이상치 없음

![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2019.png)

- mta_tax(특정 지역 통과시 세금) - 이상치 없음
    
    ![Untitled](MEET%203%200bc772662eb64dc39266ba15785535c3/Untitled%2020.png)
    

```r
# trip_distance 전처리 
dat <- dat[-which(dat$trip_distance <= 0 | dat$trip_distance >= 100),] summary(dat$trip_distance)
# passenger_count 전처리 
dat <- dat[-which(dat$passenger_count<= 0) ,]
# longitude, latitude 전처리
## 뉴욕주 경도 범위 -74.2 ~ -73.5
## 뉴욕주 위도 범위 40.45 ~ 40.95
dat <- dat[-which(dat$pickup_longitude <= -74.2 | dat$pickup_longitude >= -73.5) ,]
dat <- dat[-which(dat$pickup_latitude <= 40.45 | dat$pickup_latitude >= 40.95) ,]
dat <- dat[-which(dat$dropoff_longitude <= -74.2 | dat$dropoff_longitude >= -73.5) ,]
dat <- dat[-which(dat$dropoff_latitude <= 40.45 | dat$dropoff_latitude >= 40.95) ,]
# tip_amount 전처리 
dat <- dat[-which(dat$tip_amount < 0), ]
```thsk

## ***

---

## **[ MEET3 회의 내용 ]**

- 해올일
    - 변수선택법(K-flod, Stepwise, Best subset) 각자 하나씩 해오기
        - → 결과에 맞게 회귀분석 돌려오기
    - 보고서 EDA 까지 작성하기 + 서론 @천진영
    - ggmap 이용하여 시각화해오기 @천진영

 

보고서 구조 

변수선택 1

분석 과정 및 결과

변수선택 2

분석 과정 및 결과

변수선택 3

분석 과정 및 결과

| 방법 | auc  |  |  |  |
| --- | --- | --- | --- | --- |
| 변수선택1 |  |  |  |  |
| 변수선택2 |  |  |  |  |
| 변수선택3 |  |  |  |  |

결국은 어떤 방식이 가자 좋다 라고 마무리를 하면 어떨까