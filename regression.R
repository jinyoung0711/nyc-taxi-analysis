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
