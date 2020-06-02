######### 단계 4 ########
# 모형의 식별

par(mfrow=c(2,1))
acf(duw_ts)
pacf(duw_ts) 

par(mfrow=c(2,1))l
acf(log(duw_ts))
pacf(log(duw_ts))

par(mfrow=c(2,1))
acf(one_diff, main="ACF of Differencing")
pacf(one_diff, main="PACF of Differencing")

acf(del_seasonal, main="ACF of Seasonal Adjustment")
pacf(del_seasonal, main="PACF of Seasonal Adjustment")


acf(ds, main="ACF of differencing + seasonal adjustment")
pacf(ds, main="PACF of differencing + seasonal adjustment")


auto.arima(duw_ts) # (3,0,3)
auto.arima(log(duw_ts)) # 
auto.arima(diff(duw_ts))
auto.arima(diff(log(duw_ts)))

#auto.arima(del_seasonal)

adf.test(duw_ts)
adf.test(log(duw_ts))
adf.test(diff(duw_ts))
adf.test(diff(log(duw_ts)))

