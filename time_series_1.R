library(smooth)
library(TTR)
library(forecast)
library(tseries)

setwd("C:/Users/dohye/Desktop/timeseries")

duw<- read.csv("dust_week.csv", header=TRUE)
plot(duw$dust, type="l")

par(mfrow=c(1,1))
dt <- as.Date(duw$date)
sd<-as.Date("2012-01-01")
ed<-as.Date("2019-04-26")
dt_list <- seq(from=sd,to=ed,by="6 months") #6개월마다 그래프에 나타나도록 list 생성
dt_label <- substr(as.character(dt_list),3,7) # yyyy-mm 형태로 추출

plot(dt, duw$dust, type="l", main="Fine Dust Concentration (2012-2019)", xlab="Time", ylab="concentration(㎍/㎥)", xaxt="n")
axis(1, at=dt_list, labels = dt_label) # 원하는대로 축 설정

# 시계열로 만들기
par(mfrow=c(1,1))
duw_ts <- ts(duw$dust,start=c(2012,1,1),end=c(2019,4,21),frequency = 54) # 주별 데이터
plot(duw_ts, main="Time Series Plot of PM10", xlab="Time", ylab="concentration(㎍/㎥)")
#abline(mean(duw_ts),0,col="Red")
#abline(var(duw_ts),0,col="Blue")

# 추가로 인접한것 끼리 상관관계 있어 보이는지 확인
length(duw$dust)
length(duw_ts)
xt = duw_ts[2:382]
xt_1 = duw_ts[1:381]
cor(xt_1, xt) # 0.69
plot(xt_1,xt, main="Correlation of xt_1, xt") # 인접한것 끼리 상관이 있어 보인다

# log변환
log_ts <- log(duw_ts)
plot(log_ts)

