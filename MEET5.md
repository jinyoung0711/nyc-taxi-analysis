# MEET 5

날짜: 2022년 12월 17일 오후 6:00 (GMT+9)
분류: 정기회의
여부: 예정
참여자: 천진영, ***, ***, ***

## **[ 분석 내용 ]**

## 진영

### 전체 코드 정리

- **load library**
    
    ```r
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
    ```
    
- **full data load**
    
    ```r
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
    ```
    
- **Drop NA**
    
    ```r
    #### 결측치 제거 ####
    
    # 결측치 확인-> 184개의 NA 존재
    colSums(is.na(dat))  
    
    # 결측치가 존재하는 dropoff_latitude와 dropoff_longitude가 같은 행에 존재하는 것 확인 
    which(is.na(dat$dropoff_latitude)) == which(is.na(dat$dropoff_longitude))
    
    df <- filter(dat, is.na(dropoff_longitude))
    df$pickup_datetime == df$dropoff_datetime 
    
    # 그러면 이런 행들을 분석해보자
    df$vendor_id != 'CMT' # 다 CMT 회사인 것도 이상해
    
    df[df$pickup_datetime != df$dropoff_datetime,]
    
    df$tolls_amount == 0 
    
    # 188개의 NA 제거 후 7,919,973 Obs.
    dat <- na.omit(dat)
    ```
    
- **EDA (Exploratory Data Analysis)**
    
    ```r
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
    ```
    
- **Derived Variable**
    
    ```r
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
    ```
    
- **Drop Outlier**
    
    ```r
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
    ```
    
- **Regression**
    - 00시 - 04시 regression
        
        ```r
        #### 회귀식 추정 ####
        map.fun <- function(k, v){
          dat <- data.frame(tot = v$total_amount, dist = v$trip_distance, 
                            time = v$trip_time_in_secs, tip = v$tip_amount
                            )
          Xk <- model.matrix(tot ~ ., dat)
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
          
          beta.hat <- solve(XtX,Xty)
          nn <- XtX[1,1]
          res = list( mat = list(XtX = XtX, Xty = Xty, yty = yty),
                      stat = list(n=nn, beta.hat=beta.hat))
          keyval(1, res)
        }
        
        reg1_dat <- picktime1_dat[, c(3,2,5,6)]
        reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg1_dat),
                                              map=map.fun, 
                                              reduce=reduce.fun)))
        reg.result
        ```
        
        ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled.png)
        
    - 04시 - 08시 regression
        
        ```r
        #### 04-08시 regression ####
        reg2_dat <- picktime2_dat[, c(3,2,5,6)]
        reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg2_dat),
                                              map=map.fun, 
                                              reduce=reduce.fun)))
        reg.result
        ```
        
        ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%201.png)
        
    - *08시 - 12시 regression*
        
        ```r
        reg3_dat <- picktime3_dat[, c(3,2,5,6)]
        reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg3_dat),
                                              map=map.fun, 
                                              reduce=reduce.fun)))
        reg.result
        ```
        
    - *12시 - 16시 regression*
        
        ```r
        reg4_dat <- picktime4_dat[, c(3,2,5,6)]
        reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg4_dat),
                                              map=map.fun, 
                                              reduce=reduce.fun)))
        reg.result
        ```
        
    - *16시 - 20시 regression*
        
        ```r
        reg5_dat <- picktime5_dat[, c(3,2,5,6)]
        reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg5_dat),
                                              map=map.fun, 
                                              reduce=reduce.fun)))
        reg.result
        ```
        
    - *20시 - 24시 regression*
        
        ```r
        reg6_dat <- picktime6_dat[, c(3,2,5,6)]
        reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg6_dat),
                                              map=map.fun, 
                                              reduce=reduce.fun)))
        reg.result
        ```
        
- **Mapping**
    - full data mapping
        
        ```r
        library(RgoogleMaps)
        maps.pickinfo <- dat[, c(7, 6)]
        maps.dropinfo <- dat[, c(9, 8)]
        
        map.center.loc <- c(40.6643, -73.938)
        input_zoom <- 10
        map_data <- maps.pickinfo
        ```
        
        - pickup mapping
            
            ```r
            map <- GetMap(center = map.center.loc,
                          zoom = input_zoom, maptype = "roadmap", format = 'png8',
                          destfile= "map.png")
            
            PlotOnStaticMap(map, lat = maps.pickinfo$pickup_latitude, lon = maps.pickinfo$pickup_longitude,
                            destfile = 'map.point.png', cex = 0.5, pch = 6, col = 'red', add= FALSE)
            ```
            
            ![pickup mapping](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%202.png)
            
            pickup mapping
            
        - dropoff mapping
            
            ```r
            map <- GetMap(center = map.center.loc,
                          zoom = input_zoom, maptype = "roadmap", format = 'png8',
                          destfile= "map.png")
            
            PlotOnStaticMap(map, lat = maps.dropinfo$dropoff_latitude, lon = maps.dropinfo$dropoff_longitude,
                            destfile = 'map.point.png', cex = 0.5, pch = 20, col = 'red', add= FALSE)
            ```
            
            ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%203.png)
            
    - 00시-04시 data mapping
        
        ```r
        library(RgoogleMaps)
        maps.pickinfo <- dat[, c(7, 6)]
        maps.dropinfo <- dat[, c(9, 8)]
        
        map.center.loc <- c(40.6643, -73.938)
        input_zoom <- 10
        map_data <- maps.pickinfo
        ```
        
        ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%204.png)
        
    - 04시-08시 data mapping
        
        ```r
        maps.pickinfo <- picktime2_dat[, c(7, 6)]
        maps.dropinfo <- picktime2_dat[, c(9, 8)]
        
        map.center.loc <- c(40.6643, -73.938)
        input_zoom <- 11
        ```
        
        - pickup, dropoff mapping code
        
        ```r
        map <- GetMap(center = map.center.loc,
                      zoom = input_zoom, maptype = "roadmap", format = 'png8',
                      destfile= "map.png")
        
        PlotOnStaticMap(map, lat = maps.pickinfo$pickup_latitude, lon = maps.pickinfo$pickup_longitude,
                        destfile = 'map.point.png', cex = 0.5, pch = '.', col = 'red', add= FALSE)
        
        map <- GetMap(center = map.center.loc,
                      zoom = input_zoom, maptype = "roadmap", format = 'png8',
                      destfile= "map.png")
        
        PlotOnStaticMap(map, lat = maps.dropinfo$dropoff_latitude, lon = maps.dropinfo$dropoff_longitude,
                        destfile = 'map.point.png', cex = 0.5, pch = 20, col = 'red', add= FALSE)
        ```
        
        - pickup
        
        ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%205.png)
        
        - dropoff
    - *08시-12시 data mapping*
        
        ```r
        
        ```
        
        - pickup, dropoff mapping code
        
        ```r
        
        ```
        
        - pickup
        - dropoff
    - *12시-16시 data mapping*
        
        ```r
        
        ```
        
        - pickup, dropoff mapping code
        
        ```r
        
        ```
        
        - pickup
        - dropoff
    - *16시-20시 data mapping*
        
        ```r
        
        ```
        
        - pickup, dropoff mapping code
        
        ```r
        
        ```
        
        - pickup
        - dropoff
    - *20시-24시 data mapping*
        
        ```r
        
        ```
        
        - pickup, dropoff mapping code
        
        ```r
        
        ```
        
        - pickup
        - dropoff
- 상위 0.1% 기사님들 데이터 추출
    
    ```r
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
    
    # 188개의 NA 제거 후 8,658,800 Obs.
    dat <- na.omit(dat)
    str(dat)
    
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
    dat$pickup_day <- wday(pickup.date, label = TRUE)
    
    dropoff.date <- as.Date(dat$dropoff_datetime)
    dat$dropoff_day <- wday(dropoff.date, label = TRUE)
    
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
    str(dat) # 8658800 obs. of  15 variables
    
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
    str(dat) # 8658653 Obs. of 14 variable
    
    # total_amount 전처리
    total_amount_max <- max(dat$total_amount)
    dat <- dat[-which(dat$total_amount <= 0 | dat$total_amount == total_amount_max), ]
    
    # rate_code 전처리 
    dat <- dat[which(dat$rate_code==1), ]
    
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
    
    #### 데이터 그룹화 ####
    
    rmr.options(backend = "local")
    
    picktime1_dat <- dat[which(dat$pickup_time_bin == 'pickup1'),]
    picktime2_dat <- dat[which(dat$pickup_time_bin == 'pickup2'),]
    picktime3_dat <- dat[which(dat$pickup_time_bin == 'pickup3'),]
    picktime4_dat <- dat[which(dat$pickup_time_bin == 'pickup4'),]
    picktime5_dat <- dat[which(dat$pickup_time_bin == 'pickup5'),]
    picktime6_dat <- dat[which(dat$pickup_time_bin == 'pickup6'),]
    
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
    
    #### picktime1_dat regression #####
    
    reg1_dat <- picktime1_dat[, c(3, 2, 5, 6)]
    
    reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg1_dat),
                                            map=map.fun, 
                                            reduce=reduce.fun, combine = T)))
    
    reg.summary <- fun.summary(reg.result)
    reg.summary$stat
    reg.summary$anova
    
    # 잔차가 큰 값 선택, Design Matrix
    taxiDesingMat <- model.matrix(object =
                                    total_amount ~ trip_distance +
                                    trip_time_in_secs + tip_amount, data =
                                    reg1_dat[,c("trip_distance", "trip_time_in_secs", "tip_amount",
                                                "total_amount")])
    
    # 추정치 계산
    fittedFare <- t(reg.summary$stat$beta.hat) %*% t(taxiDesingMat)
    fittedFare <- t(fittedFare)
    resi <- reg1_dat$total_amount - fittedFare[,1]
    
    # 잔차
    BigRes <- sort(resi, decreasing = T) %>%
      names() # 잔차 INDEX
    BigResIDX <-
      BigRes[1:ceiling(length(BigRes)*0.001)] # 잔차 INDEX 중 상위 0.1%
    
    # 실제 낸 돈 - 추정한 정상금액 == 잔차
    reg1_dat[BigResIDX,] %>%
      mutate(estimateFare =
               fittedFare[,1][BigResIDX], residual =
               resi[BigResIDX]) %>% select(total_amount,
                                           estimateFare, residual) %>% head()
    reg1_dat[BigResIDX,] %>% mutate(estimateFare =
                                      fittedFare[,1][BigResIDX], residual =
                                      resi[BigResIDX]) %>% select(total_amount,
                                                                  estimateFare, residual) %>% tail()
    
    # 변수가 다 있는 데이터로 돌아옴
    selectedDataG1 <- picktime1_dat[BigResIDX, ]
    
    str(selectedDataG1)
    
    ### picktime2_dat regression ###
    reg2_dat <- picktime2_dat[, c(3, 2, 5, 6)]
    
    reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg2_dat),
                                            map=map.fun, 
                                            reduce=reduce.fun, combine = T)))
    
    reg.summary <- fun.summary(reg.result)
    reg.summary$stat
    reg.summary$anova
    
    ### picktime3_dat regression ###
    reg3_dat <- picktime3_dat[, c(3, 2, 5, 6)]
    
    reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg3_dat),
                                            map=map.fun, 
                                            reduce=reduce.fun, combine = T)))
    
    reg.summary <- fun.summary(reg.result)
    reg.summary$stat
    reg.summary$anova
    
    ### picktime4_dat regression ###
    reg4_dat <- picktime4_dat[, c(3, 2, 5, 6)]
    
    reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg4_dat),
                                            map=map.fun, 
                                            reduce=reduce.fun, combine = T)))
    
    reg.summary <- fun.summary(reg.result)
    reg.summary$stat
    reg.summary$anova
    
    ### picktime5_dat regression ###
    reg5_dat <- picktime5_dat[, c(3, 2, 5, 6)]
    
    reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg5_dat),
                                            map=map.fun, 
                                            reduce=reduce.fun, combine = T)))
    
    reg.summary <- fun.summary(reg.result)
    reg.summary$stat
    reg.summary$anova
    
    ### picktime6_dat regression ###
    reg6_dat <- picktime6_dat[, c(3, 2, 5, 6)]
    
    reg.result <- values(from.dfs(mapreduce(input=to.dfs(reg6_dat),
                                            map=map.fun, 
                                            reduce=reduce.fun, combine = T)))
    
    reg.summary <- fun.summary(reg.result)
    reg.summary$stat
    reg.summary$anova
    ```
    
- **full code**
    
    ```r
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
    
    # 188개의 NA 제거 후 8,658,800 Obs.
    dat <- na.omit(dat)
    str(dat)
    
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
    dat$pickup_day <- wday(pickup.date, label = TRUE)
    
    dropoff.date <- as.Date(dat$dropoff_datetime)
    dat$dropoff_day <- wday(dropoff.date, label = TRUE)
    
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
    str(dat) # 8658800 obs. of  15 variables
    
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
    str(dat) # 8658653 Obs. of 14 variable
    
    # total_amount 전처리
    total_amount_max <- max(dat$total_amount)
    dat <- dat[-which(dat$total_amount <= 0 | dat$total_amount == total_amount_max), ]
    
    # rate_code 전처리 
    dat <- dat[which(dat$rate_code==1), ]
    
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
    
    #### 데이터 그룹화 ####
    
    rmr.options(backend = "local")
    
    picktime1_dat <- dat[which(dat$pickup_time_bin == 'pickup1'),]
    picktime2_dat <- dat[which(dat$pickup_time_bin == 'pickup2'),]
    picktime3_dat <- dat[which(dat$pickup_time_bin == 'pickup3'),]
    picktime4_dat <- dat[which(dat$pickup_time_bin == 'pickup4'),]
    picktime5_dat <- dat[which(dat$pickup_time_bin == 'pickup5'),]
    picktime6_dat <- dat[which(dat$pickup_time_bin == 'pickup6'),]
    
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
    
    #### regression function ####
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
      res <- arr[BigResIDX, ]
      return (res)
    }
    
    selectedDataG1 <- reg.fun(picktime1_dat)
    str(selectedDataG1)
    selectedDataG2 <- reg.fun(picktime2_dat)
    str(selectedDataG2)
    selectedDataG3 <- reg.fun(picktime3_dat)
    str(selectedDataG3)
    selectedDataG4 <- reg.fun(picktime4_dat)
    selectedDataG5 <- reg.fun(picktime5_dat)
    selectedDataG6 <- reg.fun(picktime6_dat)
    ```
    
- 

### 이것저것 해보기

- [x]  전체 변수 회귀분석 돌려보기
- 왜인지 오류 에러 남 ………….. ㅠ
- [x]  latitude longitude 전처리  (그래도 다시 한번 팀원이랑 체크하기)
- [x]  factor 변수 one-hot encoding 해보고 돌려보기
    
    ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%206.png)
    
    - 음………… full data로 해서 그런가 ……….?
    - full data를 기준을 정해서 나눠서 돌려보자……..!
        - [x]  pickup_day == “월” 데이터만 추출해서 돌려보기’
            
            데이터 수 790만개 → 100만개 
            
            - 설명 변수 일부 추출
                
                ```r
                ex1 <- dat
                head(ex1)
                
                ex1 <- ex1[which(ex1$pickup_day == '월'),]
                head(ex1)
                str(ex1)
                ex1 <- ex1[, c(-10, -11)]
                
                #### 회귀식 추정 ####
                map.fun <- function(k, v){
                  dat <- data.frame(tot = v$total_amount, dist = v$trip_distance,
                                    p.lon=v$pickup_longitude,
                                    p.lat=v$pickup_latitude, d.lon=v$dropoff_longitude,
                                    d.lat = v$dropoff_latitude)
                  Xk <- model.matrix(tot ~ ., dat)
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
                  
                  beta.hat <- solve(XtX,Xty)
                  nn <- XtX[1,1]
                  res = list( mat = list(XtX = XtX, Xty = Xty, yty = yty),
                              stat = list(n=nn, beta.hat=beta.hat))
                  keyval(1, res)
                }
                
                reg.result<-values(from.dfs(mapreduce(input=to.dfs(ex1),
                                                      map=map.fun, 
                                                      reduce=reduce.fun)))
                reg.result
                ```
                
                아싸 이거다 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                
                ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%207.png)
                
            - 그럼 전체 설명변수로 돌려볼게 !!!!!!!!!
                
                ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%208.png)
                
                ㅎㅎ 아싸 ^^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                ```r
                map.fun <- function(k, v){
                  dat <- data.frame(total = v$total_amount, tip = v$tip_amount, trip_time = v$trip_time_in_secs, trip_dist = v$trip_distance, 
                                    pick_long = v$pickup_longitude, drop_long = v$dropoff_longitude, pic_lat = v$pickup_latitude, drop_lat = v$dropoff_latitude,
                                    pick1 = v$pickup1, pick2 = v$pickup2, pick3 = v$pickup3, pick4 = v$pickup4, pick5 = v$pickup5, pick6 = v$pickup6)
                
                  Xk <- model.matrix(total ~ ., dat)
                  yk <- as.matrix(dat[,1])
                  XtXk <- crossprod(Xk,Xk)
                  Xtyk <- crossprod(Xk,yk)
                  ytyk <- crossprod(yk,yk)
                  res<-list(XtXk, Xtyk, ytyk)
                  keyval(1, res)
                }
                
                str(ex1_rename)
                
                reduce.fun <- function(k, v) {
                  XtX <- Reduce("+",v[seq_along(v) %% 3 == 1])
                  Xty <- Reduce("+",v[seq_along(v) %% 3 == 2])
                  yty <- Reduce("+",v[seq_along(v) %% 3 == 0])
                  
                  beta.hat <- solve(XtX,Xty)
                  nn <- XtX[1,1]
                  res = list( mat = list(XtX = XtX, Xty = Xty, yty = yty),
                              stat = list(n=nn, beta.hat=beta.hat))
                  keyval(1, res)
                }
                
                reg.result <- values(from.dfs(mapreduce(input=to.dfs(ex1_rename),
                                                      map=map.fun, 
                                                      reduce=reduce.fun)))
                reg.result
                ```
                
                왜 자꾸 말썽인건데 !!!!!!!!!!!!!!!!!!!!
                
- [x]  Mapping 해보기
- [x]  K-means time 으로 해보기 ……….. but error …………
- [x]  전체적인 흐름 구상

### Project Overview

서론 → EDA → 데이터 정제 → Regression → Mapping → Conclusion 

Mapping하면서 분석 추가

Regression 어떻게 사용할 것인지. 생각을 해봐야함…

Regression은 보통 prediction or estimation

우리는 total_amount predict 불가능 … 그렇다면 … estimation 쪽인데…

~~ex : group 별 X에 대해 분석 : 아니다 말로 설명하자 ! 잔차를 바가지 요금이라고 판단을 했단 말이지 아 괜찮은 그런거 뭐 없을까 ?! ㅠ ㅠ~~

- 전체적인 흐름
    - 서론 내용 좀 더 고민해보기
    - EDA → 수정 X
    - 데이터 정제 → 4) 변수 선택 쪽 수정
        - 앞서 만든 파생변수들을 토대로 group화 하여 데이터 나누었다 ~ 의 흐름
    - 본론
        - regression 후 추정된 회귀식 정리 + 추가적인 변수들을 바탕으로 분석 (승객수, 승하차 위치, 뭐 탑승 하차 요일이 어떻더라 ~ )  + 가설과 비교 ex) 러시아워에 해당하는 시간에 따른 가설 ~ or 출퇴근 시간, 뭐 등등 ~ 이 시간에는 할증같은게 적용되어서 더 비싸네? 거리에 영향을 많이 받네 등등 ~ @*** @천진영
        - Mapping 관련하여 어느 관광지에서 출발 ~ 등 관광지 좌표 따서 Mapping 해주기 ( 어떤 관광지를 넣어야할까 ~~~ 생각해보기 !!!) @*** @***
    
    - 결론 → 대충 끄적이면 되니까 >>>>>!!~!
    
- ~~개소리 🐶 : 개소리에서 좋은 idea가 나온다 !!!!!!!!!!!!!!!!!ㅠ!!!~~
    
    택시기사들 돈 많이 벌라고 수요에 따른 경로 예측 → pickup time을 y로 두고 
    
    시간의 순서에 따라 경로를 잇기. 시간대별 순환버스 노선 등 다양한 주제로 가능할 것 같기도 하고 ~\
    

---

주 60시간 하루 10시간 제한이 있으니, 수입에 있어서 극한의 효율을 얻기 위해서 어떤 요일, 시간대에 근무를 해야하는지 → 결론에 녹이기.

- 이미지 모음
    
    기사님. 토요일에 쉬세요!
    
    ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%209.png)
    
    ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%2010.png)
    
    ![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%2011.png)
    
- 그룹별 pickup, dropoff 사진
    1. G1(00~04)
        
        ![G1_pickup](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G1_pickup.png)
        
        G1_pickup
        
        ![G1_dropoff](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G1_dropoff.png)
        
        G1_dropoff
        
    2. G2(04~08)
        
        ![G2_pickup](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G2_pickup.png)
        
        G2_pickup
        
        ![G2_dropoff](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G2_dropoff.png)
        
        G2_dropoff
        
    3. G3(08~12)
        
        ![G3_pickup](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G3_pickup.png)
        
        G3_pickup
        
        ![G3_dropoff](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G3_dropoff.png)
        
        G3_dropoff
        
    4. G4(12~16)
        
        ![G4_pickup](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G4_pickup.png)
        
        G4_pickup
        
        ![G4_dropoff](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G4_dropoff.png)
        
        G4_dropoff
        
    5. G5(16~20)
        
        ![G5_pickup](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G5_pickup.png)
        
        G5_pickup
        
        ![G5_dropoff](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G5_dropoff.png)
        
        G5_dropoff
        
    6. G6(20~24)
        
        ![G6_pickup](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G6_pickup.png)
        
        G6_pickup
        
        ![G6_dropoff](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/G6_dropoff.png)
        
        G6_dropoff
        

![Untitled](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/Untitled%2012.png)

![tip.png](MEET%205%20dd5ebb0df5a14567989cbf4448b3937a/tip.png)
