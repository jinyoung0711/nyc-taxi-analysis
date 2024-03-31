# MEET 2

날짜: 2022년 11월 25일 오후 7:00 (GMT+9)
분류: 정기회의
여부: 예정
참여자: 천진영, ***, ***, ***

### **[ MEET1 회의 내용 ]**

주제 정하기

- ‘어느 날씨에 어느 관광지에 많이 갈것이다’ 라는 가설 체크
- 택시 기사 관점에서 시간 대비 효율적 이동
    - 시간대 별로 언제 운전을 하는게 좋을까?
    - 어디서 출발을 해야 돈을 더 벌 수 있을까?
- 시간에 따른 특정 지역 분집도 분석

위치랑 시간 데이터 쓰면 좋을듯 .. 

---

### **[ 분석 내용 ]**

- 진영
    
    ### library load
    
    ```r
    # load library
    
    library(rhdfs)
    hdfs.init()
    library(rmr2)
    
    library(ggplot2)
    library(dplyr)
    library(psych)
    library(tidyverse)
    library(lubridate)
    library(stringr)
    library(corrplot)
    ```
    
    ### 전체 데이터 로드
    
    ```r
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
    
    str(taxi)
    dat <- taxi[, c(-1, -2, -4, -12, -15)] # medallion, hack_license, payment_type, stroe_and_fwd_flag, passenger_count 변수 삭제
    ```
    
    ## EDA
    
    ### 변수 시각화
    
    - vender_id
        
        # CMT : 4335005, VTS : 4323795
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled.png)
        
    - fare_amount
        
        ![fare_amount.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/fare_amount.jpg)
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%201.png)
        
        이상치들 제거. 0보다 작은 값들 제거  900인 자료 확인
        
    - surcharge
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%202.png)
        
        600을 넘어가는 자료 확인해볼 필요 있음. 대부분의 자료가 0에 분포
        
    - mta_tax
        
        ![mta_tax.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/mta_tax.jpg)
        
        대부분의 값이 0과 0.5사이의 값. 나머지 자료들 확인할 필요 있음. 0보다 작은 값 제거
        
    - tips_amount
    - tolls_amount
        
        ![tolls_amount.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/tolls_amount.jpg)
        
        0보다 작은 값 제거. 자료의 끝쪽 index에 tolls_amount의 값이 큰 자료들이 분포해있음. 확인해볼 필요 있음
        
    - total_amount
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%203.png)
        
        대부분이 0-200$ 사이의 값을 갖고 있으며 가장 값이 큰 1000$ 근접한 자료도 살펴볼 필요 있음. **0보다 작은 total_amount는 제거**
        
    - rate_code
        
        ![rate_code.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/rate_code.jpg)
        
        rate_code == 1인 standard rate의 비율이 가장 많음. 1 - 6 값 의외에 이상치들도 나타남 → **제거 필요**
        
    - pickup_datetime
    - dropoff_datetime
    - trim_time_in_secs
    - trip_distance
        
        
    - pickup_longitude
    - pickup_latitude
    - pickup_day
        
        ![pickup_day.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/pickup_day.jpg)
        
        월요일에 가장 pickup이 가장 적은 것을 확인할 수 있고, 목, 금, 토요일이 많음.
        
    - dropoff_day
        
        ![dropoff_day.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/dropoff_day.jpg)
        
        pickup과 동일한 분포
        
    - pickup_time_bin
        
        ![pickup_time_bin.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/pickup_time_bin.jpg)
        
        밤 시간대 21:00 ~ 23:59 가장 택시 pickup이 많고 새벽이 가장 적음.
        
    - dropoff_time_bin
        
        ![dropoff_time_bin.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/dropoff_time_bin.jpg)
        
        pickup과 같은 분포
        
    
    ### correlation heatmap
    
    ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%204.png)
    
    ### 결측치 제거
    
    ```r
    # 결측치 확인-> 188개의 NA 존재
    colSums(is.na(dat))  
    
    # 결측치가 존재하는 dropoff_latitude와 dropoff_longitude가 같은 행에 존재하는 것 확인 
    which(is.na(dat$dropoff_latitude)) == which(is.na(dat$dropoff_longitude))
    
    df <- filter(dat, is.na(dropoff_longitude))
    df$pickup_datetime == df$dropoff_datetime 
    
    # 그러면 이런 행들을 분석해보자
    df$vendor_id != 'CMT' # 다 CMT 회사인 것도 이상해(데이터를 제공한 택시 회사를 뜻하는 듯...?)
    
    df[df$pickup_datetime != df$dropoff_datetime,]
    
    df$tolls_amount == 0 
    
    # 188개의 NA 제거 후 8,658,988 Obs.
    dat <- na.omit(dat)
    
    # plot 찍어보고 데이터 확인하기
    
    ```
    
    ### 데이터 전처리
    
    ### 시간 변수 4개로 범주화
    
    ```r
    #### 데이터 전처리 ####  
    
    ## 파생변수 생성 ##
    
    # date 변수 활용에 앞서 요일, 시간대별 파생변수 생성
    # 시간대별 변수는 다음과 같이 범주화
    # 00:01 ~ 06:00 -> 새벽, 06:01 ~ 12:00 -> 오전, 12:01 ~ 18:00 -> 오후, 18:01 ~ 23:59 -> 밤
    
    # [요일 변수 생성]
    pickup.date <- as.Date(dat$pickup_datetime)
    wday(pickup.date)
    dat$pickup_day <- wday(pickup.date, label = TRUE)
    
    dropoff.date <- as.Date(dat$dropoff_datetime)
    wday(dropoff.date)
    dat$dropoff_day <- wday(dropoff.date, label = TRUE)
    
    summary(dat$pickup_day)
    summary(dat$dropoff_day)
    
    # [시간 변수 생성] 
    
    bin_fun <- function(x) {
      d1 <- x
      d1 <- str_replace_all(d1, "\\-", "")
      d1 <- str_replace_all(d1, "\\:", "")
      d1 <- as.numeric(str_sub(d1, 10, 16))
      y <- cut( x = d1, breaks = c(0, 60001, 120001, 180001, 235960), 
                             right = F, labels = c('새벽', '오전', '오후', '밤'))
    }
    
    dat$pickup_time_bin <- bin_fun(dat$pickup_datetime)
    dat$dropoff_time_bin <- bin_fun(dat$dropoff_datetime)
    
    summary(dat$pickup_time_bin)
    summary(dat$dropoff_time_bin)
    ```
    
    - **시간 변수 7개 범주화 코드**
        
        ```r
        #### 데이터 전처리 ####  
        
        ## 파생변수 생성 ##
        
        # date 변수 활용에 앞서 요일, 시간대별 파생변수 생성
        # 시간대별 변수는 다음과 같이 범주화
        # 00:01 ~ 03:00 -> 1, 03:01 ~ 06:00 → 2, 06:01 ~ 12:00 → 3, 
        # 12:01 ~ 15:00 → 4, 15:01 ~ 18:00 → 5, 18:01 ~ 21:00 -> 6, 21:01 ~ 23:59 -> 7
        
        # [요일 변수 생성]
        pickup.date <- as.Date(dat$pickup_datetime)
        wday(pickup.date)
        dat$pickup_day <- wday(pickup.date, label = TRUE)
        
        dropoff.date <- as.Date(dat$dropoff_datetime)
        wday(dropoff.date)
        dat$dropoff_day <- wday(dropoff.date, label = TRUE)
        
        summary(dat$pickup_day)
        summary(dat$dropoff_day)
        
        # [시간 변수 생성] 
        
        bin_fun <- function(x) {
          d1 <- x
          d1 <- str_replace_all(d1, "\\-", "")
          d1 <- str_replace_all(d1, "\\:", "")
          d1 <- as.numeric(str_sub(d1, 10, 16))
          y <- cut( x = d1, breaks = c(0, 30001, 60001, 90001, 120001, 150001, 180001, 210001, 235960), 
                                 right = F, labels = c('1', '2', '3', '4', '5', '6', '7'))
        }
        
        dat$pickup_time_bin <- bin_fun(dat$pickup_datetime)
        dat$dropoff_time_bin <- bin_fun(dat$dropoff_datetime)
        
        summary(dat$pickup_time_bin)
        summary(dat$dropoff_time_bin)
        ```
        
    - example graph
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%205.png)
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%206.png)
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%207.png)
        
        ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%208.png)
        
    
    ### 회귀분석 mapreduce; code
    
    ```r
    reg.map <- function(., v) {
     dat <- data.frame(total_amount =  df$total_amount, 
    								 	 trip_distance = df$trip_distance, 
    									 trip_time_in_secs = df$trip_time_in_secs)
     Xk <- model.matrix(total_amount ~ trip_distance + trip_time_in_secs, dat)
     yk <- as.matrix(dat[,1])
     XtXk <- crossprod(Xk, Xk)
     Xtyk <- crossprod(Xk, yk)
     ytyk <- crossprod(yk, yk)
     res <- list(XtXk, Xtyk, ytyk)
     keyval(1, res)
    }
    
    reg.reduce <- function(k, v) {
     XtX <- Reduce("+", v[seq_along(v) %% 3 == 1])
     Xty <- Reduce("+", v[seq_along(v) %% 3 == 2])
     yty <- Reduce("+", v[seq_along(v) %% 3 == 0])
     res <- list(XtX = XtX, Xty = Xty, yty = yty)
     keyval(1, res)
    }
    ```
    
    ```r
    # summary : beta.hat과 MSE등 분석 결과 summary
    fun.summary <- function(v) {
     XtX = v$XtX
     Xty = v$Xty
     yty = v$yty
     beta.hat = solve(XtX, Xty)
     nn = XtX[1,1]
     ysum = Xty[1]
     ybar = ysum/nn
     stat <- list(nn = nn, beta.hat = beta.hat, ysum = ysum, ybar = ybar)
     SSE = yty - crossprod(beta.hat, Xty)
     SST = yty - ysum^2/nn
     SSR = SST - SSE
     SS <- list(SSR = SSR, SSE = SSE, SST = SST)
     df.reg = dim(XtX)[1L] - 1
     df.tot = nn - 1
     df.res = df.tot - df.reg
     DF <- list(df.reg = df.reg, df.res = df.res, df.tot = df.tot)
     MSR = SSR / df.reg
     MST = SST / df.tot
     MSE = SSE / df.res
     MS <- list(MSR = MSR, MSE = MSE, MST = MST)
     f.val = MS$MSR / MS$MSE
     p.val = pf(f.val, DF$df.reg, DF$df.res, lower.tail = F)
     anova <- list(DF = DF, SS = SS, MS = MS, f.val = f.val, p.val = p.val)
     res <- list(mat = v, stat = stat, anova = anova)
    }
    ```
    
    ### 의견 정리
    
    - hack_license 변수는 taxi운전사의 면허를 나타내는 듯함.
        - 분석에서는 제한 하였으나, 분명 count가 많은 운전사도 존재할 것.
        - count가 많은 운전사는 그만큼 taxi 승객을 많이 받았다는 것이고, 이들의 특징을 뽑는다면 우리가 추구하는 효율적인 돈 버는법?의 특징을 뽑는 것이 아닐까요 ?
    - ~~입지분석~~
        - ~~뉴욕 taxi 정류장에 대한 정보 찾아보고 taxi 정류장을 어느 곳에 배치하면 좋은지~~
        - ~~위도와 경도 데이터를 바탕으로 어떤 곳에 카페가 입점하면 좋을까?~~
    - 통상 러시아워시간을 **오전에는 6시부터 9시까지로 , 오후 퇴근 러시아워 시간은 3시30분부터 7시30분까지다 -wiki-**
    - 분석기법으로는 **회귀분석**… 이랑 뭘 쓰면 좋을까 … 조금 더 고민해봐야할듯?
    
    ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%209.png)
    
- ***
    
    주제
    
    뉴욕 기사님들 돈벌기 힘들다는 내용 
    
    [https://www.donga.com/news/Inter/article/all/20211020/109811517/1](https://www.donga.com/news/Inter/article/all/20211020/109811517/1)
    
    [http://www.tbnews.co.kr/m/view.php?idx=378](http://www.tbnews.co.kr/m/view.php?idx=378)
    
    - 뉴욕 택시 기사가 돈을 더 잘 벌려면 어떻게 해야할까?
    - 같은 시간대에 택시 기사가 운전을 한다고 했을때 어느 장소에서 대기를 했다가 출발하면 더 돈을 잘 벌수 있을까?
    
    고려해야할 점 
    
    - 시간대 분류 기준
        - 러시아워 / 일반 / 야간
        - 혹은 시간대 별로 (ex. 00~06시/ 06시~ 12시)
    - 근데 기사가 양심 없이 바가지 씌운 요금들은 어떻게 처리하나?
    - 팁과 지불 방식도 고려해야할까?
    
    변수
    
    - Vender ID : TPEP가 제공하는 레코드로 여행 기록과 관련된 제공자를 나타내는 코드
        - CMT 랑 VTS
    - pickup datetime : 출발 시간
        - mm-dd-yyyy hh24:mm:ss EDT
    - dropoff datetime : 도착 시간
        - mm-dd-yyyy hh24:mm:ss EDT
    - Passenger count :  차량의 승객 수를 의미
        - 0이면 결측치
        - 근데 우리는 필요 없을듯
    - Trip distance : 택시미터에 의한 주행 거리(마일기준)
        - 너무 거리가 길다면 결측치로 취급
    - Trip time : 운행 시간
        - 너무 운행 시간이 짧거나 길면 결측치로 취급
    - Pickup longitude :  출발 경도
    - Pickup latitude : 출발 위도
    - RateCodeID :  요금 코드를 의미하는데 출발지에 따라 분류된다. 거의 1을 갖는다.
    - Store and fwd flag
    - Dropoff longitude : 도착 경도
    - Dropoff latitude : 도착 위도
    - Payment type : 지불방식
    - Fare amount : 미터기로 계산한 요금 (할증료 포함?)
    - Extra :  기타 추가 요금 (0.5달러의 러시아워와 1달러의 야간 요금만 포함)
    - MTA tax : 미터링된 사용률에 따라 자동으로 트리거되는 MTA 세금
        - 0 ~ 0.5
    - Improvement surcharge : 세금, 추가 요금 등을 의미하는듯
    - Tip amount : 팁금액
        - 팁이 0 미만이면 결측치
    - Tolls amount : 통행료 금액
    - Total amount : 승객에게 청구된 총 금액 (현금 팁 포함 X)
    
    분석 
    
    - trip time 와 trip distance가 대충 비례하는지 분석
    - 운행 빈도가 높은 출발 지점들을 지역?으로 묶기(원)
    - 거기서 trip time 혹은 trip distance 대비 total_amount 가 높은 출발 지역을 찾기
    - 근데 기사님이 추천 출발 지점에서 출발해가지구 다른 지역으로 이동하면?
        - 뉴욕시를 작은 단위로 쪼개서(예를 들어 동?구? 단위 개념으로..) 그 지역에서 가장 돈벌기 좋은 출발 지역을 선정
        - 그 지역으로 택시를 유도하도록
        
- ***
    - **RateCodeID : The final rate code in effect at the end of the trip.**
        - 1 = standard rate
            - 초기 요금 -$2.50(321.90 meters)
            - 이후 - 321.90 meters or 정차한 경우 60초 마다 $0.50
            - 20시~06시 $0.50 야간 요금 추가
            - 주중 16~20시 $1.00 러시아워 추가요금
            
        - 2 = JFK (JFK airport에서 출발)
            - 맨해튼까지 고정요금 $52
            - 주중 16~20시 $4.50 러시아워 추가요금
            
        - 3 = Newark(Newark airport에서 출발)
            - •Plus **$17.50** Newark Surcharge.
            
        - 4 = Nassau or Westchester
            - 뉴욕 도시 안에서는 표준 요금
            - 뉴욕을 벗어나 Nassau 또는 Westchester까지 2배 요금
            
        - 5 = Negotiated fare
            - 기사와 미리 상의한 가격을 지불하고 먼 곳까지 운행하는 것
        - 6 = Group ride
        
        - 공통 추가 요금
            - 96번가 남쪽 맨해튼을 시작, 종료 또는 통과하는 모든 여행에 대해 **$2.50** (노란색 택시) 추가
            - MTA주 추가 요금 $0.50
        
        [Taxi Fare](https://www.nyc.gov/site/tlc/passengers/taxi-fare.page)
        
    
    - **Rate_code 변수 중점으로 주제를 생각해보았을때**
        - 택시기사가 돈을 많이 벌기위해서는 일반 시내에서 영업을 하는 것이 아니라 공항, 시외 등을 가는 전략으로 하는 것이 좋다
        - 주의점 - 다시 뉴욕 시내로 돌아오는 비용 또는 승객을 기다리는 시간 같은 요소는 고려 안함
    
    ```r
    #rate id 별 평균 요금, 운행 거리 구하기
    dat$rate_code
    table(dat$rate_code)
    str(dat$rate_code)
    #head(dat[dat$rate_code==0,],100)
    
    # rate_code, total_amount, trip_distance 3개의 변수를 갖는 ndat 생성
    ndat <- dat[c("rate_code", "total_amount", "trip_distance")]
    head(ndat, 20)
    levels(ndat$rate_code)
    table(ndat$rate_code)
    
    # ID별 요금, 거리 평균 구하기
    v1<-tapply(X = ndat$total_amount, INDEX = ndat$rate_code, FUN=mean); v1
    v2<-tapply(X = ndat$trip_distance, INDEX = ndat$rate_code, FUN=mean); v2
    
    # ID 1~6만 추출
    v1 <- v1[2:7]; v1
    v2 <- v2[2:7]; v2
    
    rcdat <- cbind(v1, v2)
    row.names(rcdat) <- c(paste0("codeID", 1:6))
    colnames(rcdat) <- c("amountMean", "distanceMean")
    rcdat <- as.data.frame(rcdat)
    str(rcdat)
    class(rcdat)
    #amount per distance
    rcdat$apd <- round(rcdat$amountMean/rcdat$distanceMean, 2)
    rcdat
    ```
    
    ![rcdat.PNG](MEET%202%200938ad2ca8394024be5ea852a062201f/rcdat.png)
    
    - 단순 거리당 요금을 따져보았을 때 RateID 5( 기사와 승객이 미리 합의된 금액으로 장거리 운행을 하는 것) 이 제일 효율이 높음
    - rateID별 소비자(승객)의 비율, 승객을 태우지 않았을 경우를 고려하지 않았을 때 기사는 rateID 5 손님을 많이 태우는 것이 이득이다.
        - 생각해 볼 수 있는 주제 방향
            - 가설 - rateID 5 요금을 지불하여 택시를 이용하는 승객들은 공통된 이유를 가질 것이다.
                - ex) 뭐…특정 요일에 이벤트(공연 등등…)나 항공 수…?
            - 전략 - rateID 별 시간대, 위치 분석
        
        참고한 페이지 - [http://rstudio-pubs-static.s3.amazonaws.com/414401_4e3c51fe17bc4a8f839824b88dee2e6b.html](http://rstudio-pubs-static.s3.amazonaws.com/414401_4e3c51fe17bc4a8f839824b88dee2e6b.html)
        
        - **서론에 쓰면 좋을 것 같은 내용**
            - 뉴욕 택시 기사 하루 최대 10시간 근무, 주 60 시간 근무 가능
                
                출처 - [https://www.nyc.gov/site/tlc/about/fatigued-driving-prevention-frequently-asked-questions.page](https://www.nyc.gov/site/tlc/about/fatigued-driving-prevention-frequently-asked-questions.page)
                
            - 택시기사 평균 급여(예상치, 2013년과 차이가 있을 수 있다)
                
                출처 - [https://www.indeed.com/career/taxi-driver/salaries/New-York--NY](https://www.indeed.com/career/taxi-driver/salaries/New-York--NY)
                
- ***
    
    [https://wikidocs.net/34032](https://wikidocs.net/34032)   - R 데이터 분석 및 통계 이론 관련
    
    요일별 범주를 평일 / 휴일(주말)로 나누어서 생각해도 괜찮을듯? 
    
    시간대에 따른 효율적인 택시 운용을 주제로 생각해 본다면,  뉴욕시를 몇개의 섹터로 분할한 다음(위/경도 이용) 섹터들마다 손님들이 픽업(출발)하는 카운트를 해보면 좀 효과적으로 얻을 수 있지않을까?   
    
    입지분석 관련 주제를 생각해 본다면, 앞 선 내용과 비슷하게 섹터들마다 손님들의 drop off를 카운트해보면 어떨까,,,(물론, 시간대에 따라 다양한 양상을 보일수도 있지만) 
    
    회귀분석, 유사도에 따 군집분석(k-mean 알고리즘)
    

---

### **[ MEET2 회의 내용 ]**

### 사용할 변수

- 분석에 사용
    - hack_license
    - Vender ID : A code indicating the TPEP provider that provided the record. →
    - pickup_time_bin → 4시간 기준으로 범주화
    - dropoff_time_bin → 4시간 기준으로 범주화
    - Passenger count : The number of passengers in the vehicle. This is a driver-entered value. → 차량의 승객 수를 의미
        - 새벽에 돌려보고 결과 알려주기
    - Trip distance : The elapsed trip distance in miles reported by the taximeter. → 택시미터에 의한 주행 거리(마일기준)를 의미
    - RateCodeID : The final rate code in effect at the end of the trip. → 여행이 끝날 때 적용되는 최종 요금 코드 → 각 값들은 출발지를 의미
    - 1 = standard rate
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
    
    ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled.png)
    
- tips_amount → 확인해보기  @천진영
    
    
- total_amount → 0보다 작은 값 제거, max값 제거 @*** @***
    
    ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%203.png)
    
    대부분이 0-200$ 사이의 값을 갖고 있으며 가장 값이 큰 1000$ 근접한 자료도 살펴볼 필요 있음. **0보다 작은 total_amount는 제거**
    
- rate_code == 1인 값만 사용, 나머지 값은 제거@*** @***
    
    ![rate_code.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/rate_code.jpg)
    
    rate_code == 1인 standard rate의 비율이 가장 많음. 1 - 6 값 의외에 이상치들도 나타남 → **제거 필요**
    
- trip_time_in_secs > 4000000 인 값들 제거하기 (제거하고 분석에는 사용 X - 이상치 제거에만 사용) @*** @***
    
    ![Untitled](MEET%202%200938ad2ca8394024be5ea852a062201f/Untitled%2010.png)
    
- trip_distance → 확인해보기 @천진영
    
    
- pickup_day → 전처리 X
    
    ![pickup_day.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/pickup_day.jpg)
    
    월요일에 가장 pickup이 가장 적은 것을 확인할 수 있고, 목, 금, 토요일이 많음.
    
- dropoff_day → 전처리 X
    
    ![dropoff_day.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/dropoff_day.jpg)
    
    pickup과 동일한 분포
    
- pickup_time_bin → 4시간 기준으로 범주화 @*** @***
    
    ![pickup_time_bin.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/pickup_time_bin.jpg)
    
    밤 시간대 21:00 ~ 23:59 가장 택시 pickup이 많고 새벽이 가장 적음.
    
- dropoff_time_bin → 4시간 기준으로 범주화  @*** @***
    
    ![dropoff_time_bin.jpg](MEET%202%200938ad2ca8394024be5ea852a062201f/dropoff_time_bin.jpg)
    
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
