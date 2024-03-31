# MEET 4

날짜: 2022년 12월 6일 오후 9:00 (GMT+9)
분류: 정기회의
여부: 예정
참여자: 천진영, ***, ***, ***

## **[ MEET3 회의 내용 ]**

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

---

## **[ 분석 내용 ]**

## 진영

- ggmap

## ***

- 모든 변수가 포함된 모델에서 출발하여 기준 통계치에 가장 도움이 되지 않는 변수를 삭제하거나, 모델에서 빠져 있는 변수 중에서 기준 통계치를 가장 개선시키는 변수를 추가한다.
- 이러한 변수의 추가 또는 삭제를 반복. 반대로 절편만 포함된 모델에서 출발해 변수의 추가, 삭제를 반복할 수도 있다.
- 결론적으로 선택된 변수 중 유의미한 변수 남기고 제거
- 종속 변수는 total_amount
- AIC 는 데이터셋에 대한 통계 모델의 상대적인 품질 의미
- RSS 와 AIC 가 작을수록 더 좋은 모델이다

```r
dat_sample <- dat[c(1:100000),]
dat_sample <- subset(dat_sample, select=-c(rate_code))
str(dat_sample)

m<- lm(dat_sample$total_amount ~ ., data=dat_sample)
summary(m)

#m2 <- step(lm(dat_sample$total_amount ~1, dat_sample), scope = list(upper = ~.), direction = "both")
m2 <- step(m,direction='both')

formula(m2) # 최종 모델
```

```java
Start:  AIC=215864.9
dat_sample$total_amount ~ vendor_id + passenger_count + trip_distance + 
    pickup_longitude + pickup_latitude + dropoff_longitude + 
    dropoff_latitude + pickup_time_bin + dropoff_time_bin + pickup_day + 
    dropoff_day

                    Df Sum of Sq     RSS    AIC
- passenger_count    1        17  865441 215865
<none>                            865424 215865
- vendor_id          1        43  865467 215868
- dropoff_day        6       773  866197 215942
- pickup_day         6      1139  866563 215985
- dropoff_latitude   1      2396  867820 216139
- dropoff_longitude  1      3229  868653 216235
- pickup_time_bin    5      3780  869204 216291
- pickup_longitude   1      3869  869293 216309
- dropoff_time_bin   5      4423  869847 216365
- pickup_latitude    1      4747  870171 216410
- trip_distance      1   5591360 6456784 416830

Step:  AIC=215864.9
dat_sample$total_amount ~ vendor_id + trip_distance + pickup_longitude + 
    pickup_latitude + dropoff_longitude + dropoff_latitude + 
    pickup_time_bin + dropoff_time_bin + pickup_day + dropoff_day

                    Df Sum of Sq     RSS    AIC
<none>                            865441 215865
+ passenger_count    1        17  865424 215865
- vendor_id          1        68  865509 215871
- dropoff_day        6       773  866214 215942
- pickup_day         6      1140  866581 215984
- dropoff_latitude   1      2395  867836 216139
- dropoff_longitude  1      3228  868669 216235
- pickup_time_bin    5      3781  869222 216291
- pickup_longitude   1      3868  869309 216309
- dropoff_time_bin   5      4421  869862 216364
- pickup_latitude    1      4745  870186 216410
- trip_distance      1   5591962 6457403 416837
```

- 아무것도 제거하지 않았을때가 모델 적합성이 제일 좋고 그 다음이 passenger_count 변수를 제거하는 모델이 적합성이 좋다..
- 그런데 passenger_count 를 제거해도 AIC 가 동일하다
- 진짜 더럽게 안돌아간다.. 심지어 오리지날 데이터도 아님 → 그냥 데이터 Rstudio 에서 돌리면 안되나?

## ***

```r
ddat<-dat

### k-fold ###

#library(glmnet) <- 불가
#library(cvTools) <- 불가

install.packages("glmnet")
library(boot)

ddat <- subset(ddat, select=-c(rate_code, medallion, hack_license, vendor_id, passenger_count))

model1 <- lm(ddat$total_amount ~ ., data = ddat )
summary(model1)

###case1
CV <- function(dats, n.folds){
  folds <- list() # flexible object for storing folds
  fold.size <- nrow(dats)/n.folds
  remain <- 1:nrow(dats) # all obs are in
  lm.model <- data.frame()
  
  for (i in 1:n.folds){
    select <- sample(remain, fold.size, replace = FALSE)
    #randomly sample “fold_size” from the ‘remaining observations’
    
    folds[[i]] <- select # store indices
    #write a special statement for the last fold — if there are ‘leftover points’
    
    if (i == n.folds){
      folds[[i]] <- remain
    }
    
    #update remaining indices to reflect what was taken out
    remain <- setdiff(remain, select)
    remain
  }
  
  for (i in 1:n.folds){
    # fold i
    indis <- folds[[i]] #unpack into a vector
    train <- dats[-indis, ] #split into train and test sets
    test <- dats[indis, ]
    
    lm.model[[i]] <- lm(as.numeric(total_amount) ~ ., data = test)
    #pred <- predict(lm.model, newdata = test)
    #RMSE<- mean((test$total_amount - pred)^2)
    #results[[i]] <- RMSE
  }
  return(lm.model)
}

CV(ddat, 10)

###case2

#Randomly shuffle the data
ddat <- ddat[sample(nrow(ddat)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(ddat)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation
testmodel <- list()
trainmodel <- list()
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- ddat[testIndexes, ]
  trainData <- ddat[-testIndexes, ]
  testmodel[[i]] <- lm(testData$total_amount ~ ., data = testData )
  trainmodel[[i]] <- lm(trainData$total_amount ~ ., data = trainData )
}

str(testData)
str(trainData)

summary(testmodel)
summary(trainmodel)

###case3

## 10-fold CV
# A vector for collecting the errors.
cv.error10=rep(0,5)
# The polynomial degree
degree=1:5
# A fit for each degree
for(d in degree){
  glm.fit <- glm( ddat$total_amount ~ ., data=ddat )
  cv.error10[d]=cv.glm(ddat, glm.fit, K=5)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")
summary(cv.error10)
```

## ***

Best Subset Selection

```r
# Best Subset Selection

#install.packages("leaps")
library(leaps)

#문자형은 지우자
r_dat <- dat[,c(-1,-2)]
str(r_dat)

# 변수를 모두 적합하기에는...
bestsub.model <- regsubsets(total_amount~.,data = r_dat, nvmax = 40)

bestsub.model1 <- regsubsets(total_amount  ~
                              trip_distance +
                              pickup_longitude +
                              pickup_latitude +
                              pickup_time_bin +
                              pickup_day 
                            , data = r_dat , nvmax = 14)
  
bestsub.model2 <- regsubsets(total_amount  ~
                               trip_distance +
                               dropoff_longitude +
                               dropoff_latitude +
                               dropoff_time_bin +
                               dropoff_day
                             , data = r_dat, nvmax = 14) 

bestsub.model3 <- regsubsets(total_amount  ~
                               rate_code +
                               vendor_id +
                               passenger_count +
                               trip_distance 
                               
                             , data = r_dat , nvmax = 14)

options(max.print = 1000000)

summary(bestsub.model)
summary(bestsub.model1)
summary(bestsub.model2)
summary(bestsub.model3)

reg.summary <- summary(bestsub.model)
names(reg.summary)

reg.summary1 <- summary(bestsub.model1)
names(reg.summary1)

reg.summary2 <- summary(bestsub.model2)
names(reg.summary2)

reg.summary3 <- summary(bestsub.model3)
names(reg.summary3)

#binding
cbind( 
  Cp     = reg.summary$cp,
  r2   = reg.summary$rsq,
  Adj_r2 = reg.summary$adjr2,
  BIC    = reg.summary$bic,
  RSS    = reg.summary$rss 
)

cbind( 
  Cp_1     = reg.summary1$cp,
  r2_1   = reg.summary1$rsq,
  Adj_r2_1 = reg.summary1$adjr2,
  BIC_1    = reg.summary1$bic,
  RSS_1    = reg.summary1$rss 
)

cbind( 
  Cp_2     = reg.summary2$cp,
  r2_2   = reg.summary2$rsq,
  Adj_r2_2 = reg.summary2$adjr2,
  BIC_2    = reg.summary2$bic,
  RSS_2    = reg.summary2$rss 
)

cbind( 
  Cp_3     = reg.summary3$cp,
  r2_3   = reg.summary3$rsq,
  Adj_r2_3 = reg.summary3$adjr2,
  BIC_3    = reg.summary3$bic,
  RSS_3    = reg.summary3$rss 
)

#plot

which.max(reg.summary$rsq)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

par(mfrow=c(2,2))

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l") # Adj R^2
points(26,reg.summary1$adjr2[26], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l') 
points(26,reg.summary1$cp[26],col="red",cex=2,pch=20)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(26,reg.summary$bic[26],col="red",cex=2,pch=20)

coef(bestsub.model, 26)

#plot1
which.max(reg.summary1$rsq)
which.max(reg.summary1$adjr2)
which.min(reg.summary1$cp)
which.min(reg.summary1$bic)

par(mfrow=c(2,2))

plot(reg.summary1$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary1$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l") # Adj R^2
points(14,reg.summary1$adjr2[14], col="red",cex=2,pch=20)

plot(reg.summary1$cp,xlab="Number of Variables",ylab="Cp",type='l') 
points(14,reg.summary1$cp[14],col="red",cex=2,pch=20)

plot(reg.summary1$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(14,reg.summary1$bic[14],col="red",cex=2,pch=20)

coef(bestsub.model1, 14)

#plot2
which.max(reg.summary2$rsq)
which.max(reg.summary2$adjr2)
which.min(reg.summary2$cp)
which.min(reg.summary2$bic)

par(mfrow=c(2,2))

plot(reg.summary2$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary2$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l") # Adj R^2
points(14,reg.summary2$adjr2[14], col="red",cex=2,pch=20)

plot(reg.summary2$cp,xlab="Number of Variables",ylab="Cp",type='l') 
points(14,reg.summary2$cp[14],col="red",cex=2,pch=20)

plot(reg.summary2$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(14,reg.summary2$bic[14],col="red",cex=2,pch=20)

coef(bestsub.model2, 14)

#plot3
which.max(reg.summary3$rsq)
which.max(reg.summary3$adjr2)
which.min(reg.summary3$cp)
which.min(reg.summary3$bic)

par(mfrow=c(2,2))

plot(reg.summary3$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(reg.summary3$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l") # Adj R^2
points(3,reg.summary3$adjr2[3], col="red",cex=2,pch=20)

plot(reg.summary3$cp,xlab="Number of Variables",ylab="Cp",type='l') 
points(3,reg.summary3$cp[3],col="red",cex=2,pch=20)

plot(reg.summary3$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(3,reg.summary3$bic[3],col="red",cex=2,pch=20)

coef(bestsub.model3, 3)

# => vendor_id 랑 Passenger_count가 적합되지만 빼야하지 않을까...
#final model from exhaustive search
final.model <- lm(total_amount  ~
                    trip_distance +
                    pickup_longitude +
                    pickup_latitude +
                    pickup_time_bin +
                    pickup_day + 
                    dropoff_longitude +
                    dropoff_latitude +
                    dropoff_time_bin +
                    dropoff_day 
                  , data = r_dat)

summary(final.model)

#  => vendor_id 랑 Passenger_count가 적합
#final model from exhaustive search
final.model <- lm(total_amount  ~
			    vendor_id +
                    passenger_count +
                    trip_distance +
                    pickup_longitude +
                    pickup_latitude +
                    pickup_time_bin +
                    pickup_day + 
                    dropoff_longitude +
                    dropoff_latitude +
                    dropoff_time_bin +
                    dropoff_day 
                  , data = r_dat)

summary(final.model)

```

```
> summary(bestsub.model1)

Subset selection object
Call: regsubsets.formula(total_amount ~ trip_distance + pickup_longitude + 
    pickup_latitude + pickup_time_bin + pickup_day, data = r_dat, 
    nvmax = 20)
14 Variables  (and intercept)
                         Forced in Forced out
trip_distance                FALSE      FALSE
pickup_longitude             FALSE      FALSE
pickup_latitude              FALSE      FALSE
pickup_time_bin04시-08시     FALSE      FALSE
pickup_time_bin08시-12시     FALSE      FALSE
pickup_time_bin12시-16시     FALSE      FALSE
pickup_time_bin16시-20시     FALSE      FALSE
pickup_time_bin20시-24시     FALSE      FALSE
pickup_day.L                 FALSE      FALSE
pickup_day.Q                 FALSE      FALSE
pickup_day.C                 FALSE      FALSE
pickup_day^4                 FALSE      FALSE
pickup_day^5                 FALSE      FALSE
pickup_day^6                 FALSE      FALSE
1 subsets of each size up to 14
Selection Algorithm: exhaustive
          trip_distance pickup_longitude
1  ( 1 )  "*"           " "             
2  ( 1 )  "*"           " "             
3  ( 1 )  "*"           " "             
4  ( 1 )  "*"           " "             
5  ( 1 )  "*"           " "             
6  ( 1 )  "*"           " "             
7  ( 1 )  "*"           " "             
8  ( 1 )  "*"           "*"             
9  ( 1 )  "*"           "*"             
10  ( 1 ) "*"           "*"             
11  ( 1 ) "*"           "*"             
12  ( 1 ) "*"           "*"             
13  ( 1 ) "*"           "*"             
14  ( 1 ) "*"           "*"             
          pickup_latitude pickup_time_bin04시-08시
1  ( 1 )  " "             " "                     
2  ( 1 )  " "             " "                     
3  ( 1 )  " "             "*"                     
4  ( 1 )  " "             "*"                     
5  ( 1 )  " "             " "                     
6  ( 1 )  " "             "*"                     
7  ( 1 )  " "             "*"                     
8  ( 1 )  " "             "*"                     
9  ( 1 )  "*"             "*"                     
10  ( 1 ) "*"             "*"                     
11  ( 1 ) "*"             "*"                     
12  ( 1 ) "*"             "*"                     
13  ( 1 ) "*"             "*"                     
14  ( 1 ) "*"             "*"                     
          pickup_time_bin08시-12시
1  ( 1 )  " "                     
2  ( 1 )  " "                     
3  ( 1 )  " "                     
4  ( 1 )  " "                     
5  ( 1 )  "*"                     
6  ( 1 )  "*"                     
7  ( 1 )  "*"                     
8  ( 1 )  "*"                     
9  ( 1 )  "*"                     
10  ( 1 ) "*"                     
11  ( 1 ) "*"                     
12  ( 1 ) "*"                     
13  ( 1 ) "*"                     
14  ( 1 ) "*"                     
          pickup_time_bin12시-16시
1  ( 1 )  " "                     
2  ( 1 )  " "                     
3  ( 1 )  " "                     
4  ( 1 )  " "                     
5  ( 1 )  "*"                     
6  ( 1 )  "*"                     
7  ( 1 )  "*"                     
8  ( 1 )  "*"                     
9  ( 1 )  "*"                     
10  ( 1 ) "*"                     
11  ( 1 ) "*"                     
12  ( 1 ) "*"                     
13  ( 1 ) "*"                     
14  ( 1 ) "*"                     
          pickup_time_bin16시-20시
1  ( 1 )  " "                     
2  ( 1 )  " "                     
3  ( 1 )  " "                     
4  ( 1 )  "*"                     
5  ( 1 )  "*"                     
6  ( 1 )  "*"                     
7  ( 1 )  "*"                     
8  ( 1 )  "*"                     
9  ( 1 )  "*"                     
10  ( 1 ) "*"                     
11  ( 1 ) "*"                     
12  ( 1 ) "*"                     
13  ( 1 ) "*"                     
14  ( 1 ) "*"                     
          pickup_time_bin20시-24시 pickup_day.L
1  ( 1 )  " "                      " "         
2  ( 1 )  " "                      " "         
3  ( 1 )  " "                      " "         
4  ( 1 )  " "                      " "         
5  ( 1 )  " "                      " "         
6  ( 1 )  " "                      " "         
7  ( 1 )  " "                      "*"         
8  ( 1 )  " "                      "*"         
9  ( 1 )  " "                      "*"         
10  ( 1 ) "*"                      "*"         
11  ( 1 ) "*"                      "*"         
12  ( 1 ) "*"                      "*"         
13  ( 1 ) "*"                      "*"         
14  ( 1 ) "*"                      "*"         
          pickup_day.Q pickup_day.C pickup_day^4
1  ( 1 )  " "          " "          " "         
2  ( 1 )  "*"          " "          " "         
3  ( 1 )  "*"          " "          " "         
4  ( 1 )  "*"          " "          " "         
5  ( 1 )  "*"          " "          " "         
6  ( 1 )  "*"          " "          " "         
7  ( 1 )  "*"          " "          " "         
8  ( 1 )  "*"          " "          " "         
9  ( 1 )  "*"          " "          " "         
10  ( 1 ) "*"          " "          " "         
11  ( 1 ) "*"          "*"          " "         
12  ( 1 ) "*"          "*"          "*"         
13  ( 1 ) "*"          "*"          "*"         
14  ( 1 ) "*"          "*"          "*"         
          pickup_day^5 pickup_day^6
1  ( 1 )  " "          " "         
2  ( 1 )  " "          " "         
3  ( 1 )  " "          " "         
4  ( 1 )  " "          " "         
5  ( 1 )  " "          " "         
6  ( 1 )  " "          " "         
7  ( 1 )  " "          " "         
8  ( 1 )  " "          " "         
9  ( 1 )  " "          " "         
10  ( 1 ) " "          " "         
11  ( 1 ) " "          " "         
12  ( 1 ) " "          " "         
13  ( 1 ) "*"          " "         
14  ( 1 ) "*"          "*"
```

```r
> cbind( 
+   Cp_1     = reg.summary1$cp,
+   r2_1   = reg.summary1$rsq,
+   Adj_r2_1 = reg.summary1$adjr2,
+   BIC_1    = reg.summary1$bic,
+   RSS_1    = reg.summary1$rss 
+ )
              Cp_1      r2_1  Adj_r2_1     BIC_1
 [1,] 453942.93186 0.8651070 0.8651070 -16570680
 [2,] 332339.66152 0.8669869 0.8669869 -16686754
 [3,] 216838.06786 0.8687725 0.8687725 -16798533
 [4,] 134607.93783 0.8700438 0.8700437 -16879039
 [5,]  90041.73484 0.8707328 0.8707327 -16922994
 [6,]  54298.15367 0.8712854 0.8712853 -16958415
 [7,]  36679.72630 0.8715578 0.8715576 -16975923
 [8,]  24222.99139 0.8717504 0.8717502 -16988320
 [9,]  14849.62007 0.8718953 0.8718951 -16997658
[10,]   6503.76161 0.8720243 0.8720242 -17005979
[11,]   2932.16856 0.8720796 0.8720794 -17009535
[12,]    374.33229 0.8721192 0.8721190 -17012078
[13,]     31.86152 0.8721245 0.8721243 -17012406
[14,]     15.00000 0.8721248 0.8721246 -17012409
         RSS_1
 [1,] 91600212
 [2,] 90323638
 [3,] 89111118
 [4,] 88247872
 [5,] 87780009
 [6,] 87404763
 [7,] 87219789
 [8,] 87089001
 [9,] 86990582
[10,] 86902949
[11,] 86865434
[12,] 86838562
[13,] 86834946
[14,] 86834748
```

![빅분컴1.jpg](MEET%204%20555f9808922d4994bf41dd7dc56daea7/%25EB%25B9%2585%25EB%25B6%2584%25EC%25BB%25B41.jpg)

```r
> coef(bestsub.model1, 14)

             (Intercept)            trip_distance 
           -610.18194306               3.24057046 
        pickup_longitude          pickup_latitude 
             -5.91272151               4.34806703 
pickup_time_bin04시-08시 pickup_time_bin08시-12시 
             -0.57901428               0.96806427 
pickup_time_bin12시-16시 pickup_time_bin16시-20시 
              1.10227997               1.49147937 
pickup_time_bin20시-24시             pickup_day.L 
              0.38803846               0.38504839 
            pickup_day.Q             pickup_day.C 
             -0.97204731              -0.17141578 
            pickup_day^4             pickup_day^5 
             -0.14943562              -0.05501051 
            pickup_day^6 
             -0.01287816
```

Validation Set 접근법을 사용한 모델 선택 ( 모델1만 수행 함) 

```r
s_dat <- r_dat[,-c(1,3,4,8,9,11,13)]

str(s_dat)

set.seed (1)
train = sample(c(TRUE,FALSE), nrow(s_dat),rep=TRUE)
test =(! train )

regfit.best <- regsubsets(total_amount  ~
                            trip_distance +
                            pickup_longitude +
                            pickup_latitude +
                            pickup_time_bin +
                            pickup_day 
                          , data = s_dat[train,], nvmax = 14)

test.mat = model.matrix(total_amount  ~
                          trip_distance +
                          pickup_longitude +
                          pickup_latitude +
                          pickup_time_bin +
                          pickup_day 
                        , data = s_dat[test,])

val.errors = rep(NA,14)

for (i in 1:14){
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((s_dat$total_amount[test]-pred)^2)
}

which.min(val.errors)

#install.packages("ggvis")
library(ggvis)

verr <- as.data.frame(val.errors);  names(verr) <- "err"
index <- c(1:nrow(verr))
verr <- cbind.data.frame(verr,index)

verr %>% 
  ggvis(x=~ index, y=~err ) %>%
  layer_points(fill = ~ err , size =~ err ) %>%
  layer_lines(stroke := "skyblue")%>%
  add_axis("y", title = "MSE") %>% 
  add_axis("x", title = "Number of variables")

rss <- as.data.frame(sqrt(regfit.best$rss[-1]/1000000)); names(rss) <- "rss"
verr <- cbind.data.frame(verr,rss)

verr %>% 
  ggvis(x=~ index) %>%
  layer_points(y=~rss ,fill = ~ rss , size =~ rss ) %>%
  layer_lines(y=~rss ,stroke :="purple")%>%
  add_axis("y", title = "MSE & RSS/1000000  ") %>% 
  add_axis("x", title = "Number of variables") %>%
  layer_points(y=~ err, fill = ~ err , size =~ err ) %>%
  layer_lines(y=~ err, stroke := "skyblue")
```

```r
> which.min(val.errors)
[1] 14
```

![빅분컴2.jpg](MEET%204%20555f9808922d4994bf41dd7dc56daea7/%25EB%25B9%2585%25EB%25B6%2584%25EC%25BB%25B42.jpg)

![빅분컴3.jpg](MEET%204%20555f9808922d4994bf41dd7dc56daea7/%25EB%25B9%2585%25EB%25B6%2584%25EC%25BB%25B43.jpg)

---

## **[ MEET4 회의 내용 ]**