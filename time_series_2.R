######### 단계 3 ########
# 정상화 : 정상화는 분산안정화먼저, 그 다음 평균안정화 한다.

# 3-1) 분산의 정상화 (log변환)
par(mfrow=c(2,1))
plot(duw_ts, main="raw data", xlab="Time", ylab="concentration(㎍/㎥)")
plot(log_ts , main="log transform", xlab="Time", ylab="concentration(㎍/㎥)") 
# 분산은 비교적 일정해진듯 한데, 평균은 여전히 일정하지 않아보임

# 3-2) 평균 정상화
# 분해법
plot(decompose(log_ts))
plot(decompose(log_ts)$trend)
plot(decompose(log_ts)$seasonal)

## 추세제거 1 (decompose에 있는걸로)
del_trend <- log_ts - decompose(log_ts)$trend
plot(log_ts, main="Dust (log transform)", xlab="Time", ylab="concentration(㎍/㎥)")
plot(del_trend, main="Detrending by Moving Average", xlab="Time", ylab="concentration(㎍/㎥)") 
# 이동평균하니까 잃는 데이터도 많고, 추세제거해도 변함이 없음

## 추세제거 2 (linear regression 잔차로)
length(log_ts)
y <- 1:382
del_trend2 <- lm(log_ts~y)
plot(log_ts, main="Dust (log transform)", xlab="Time", ylab="concentration(㎍/㎥)")
plot(del_trend2$residuals, type="l", main="Detrending by Linear Regression", xlab="Time", ylab="concentration(㎍/㎥)") 
# 역시나 변함 없음

## 추세제거 3 (differencing 차분)
length(log_ts)
one_diff = diff(log_ts)
# one_diff= log_ts[-1]-log_ts[-382]
plot(log_ts, main="Dust (log transform)", xlab="Time", ylab="concentration(㎍/㎥)")
plot(diff(log_ts), type="l", main="first difference", xlab="Time", ylab="concentration(㎍/㎥)") # log 변환 + 1차 차분

## 계절조정
del_seasonal <- log_ts - decompose(log_ts)$seasonal

par(mfrow=c(2,1))
plot(log_ts, main="Dust (log transform)", xlab="Time", ylab="concentration(㎍/㎥)")
plot(del_seasonal, main="Seasonal Adjustment", xlab="Time", ylab="concentration(㎍/㎥)") 

# diff vs seasonal
plot(diff(log_ts), type="l", main="first difference", xlab="Time", ylab="concentration(㎍/㎥)") # log 변환 + 1차 차분
plot(del_seasonal, main="Seasonal Adjustment", xlab="Time", ylab="concentration(㎍/㎥)") # 계절조정

# 계절조정 + 차분
plot(diff(del_seasonal))


## 예측(지수평활)
## holtwinters
plot(HoltWinters(log_ts))
plot(forecast(HoltWinters(log_ts)))
hw <- forecast(HoltWinters(log_ts))
