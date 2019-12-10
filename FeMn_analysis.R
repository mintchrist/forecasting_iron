metal = read.csv("CRU Monthly Prices1.csv")
tsmetal = ts(metal$HC.FeMn,start=c(2014,4), freq = 12)
tsmetal
plot(tsmetal, xlab="Time", ylab="Price of Mn", main="Monthly Mn Price")
autoplot(tsmetal) +
  ggtitle("Monthly Mn Price") +
  xlab("Time") +
  ylab("Price of Mn")

ggseasonplot(tsmetal, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price of Mn") +
  ggtitle("Seasonal plot for Mn price")

ggseasonplot(tsmetal, polar=TRUE) +
  ylab("Price of Mn") +
  ggtitle("Seasonal plot for Mn price")

ggsubseriesplot(tsmetal) +
  ylab("Price of Mn") +
  ggtitle("Seasonal plot for Mn price")

add.decomp = decompose(tsmetal, "additive")
autoplot(add.decomp)


#-Auto correlation

ggAcf(tsmetal, plot=FALSE)

ggAcf(tsmetal)

ggAcf(tsmetal, lag.max=48)


#- White noise

res = residuals(naive(tsmetal))
res
autoplot(res)
ggAcf(res)
checkresiduals(naive(res))

ggPacf(tsmetal, lag.max=48)

ggPacf(tsmetal, lag.max=100)

ggPacf(tsmetal, lag.max=48)


#-Forecasting

tsmetal_train = window(tsmetal, start = c(2014,4), end = c(2018,6))
tsmetal_test = window(tsmetal, start = c(2018,7))

fit.ets = ets(tsmetal_train)
fit.arima = auto.arima(tsmetal_train, seasonal=FALSE)
fit.sarima = auto.arima(tsmetal_train)


summary(fit.arima)
summary(fit.sarima)


metal.naive = naive(tsmetal_train, h=30)
metal.ave   = meanf(tsmetal_train, h=30) 
metal.drift = rwf(tsmetal_train, drift=TRUE, h=30)
metal.sma   = sma(tsmetal_train, h=30)
metal.ses   = ses(tsmetal_train, h=30)
metal.ets = forecast(fit.ets, h=30)
metal.arima  = forecast(fit.arima, h=length(tsmetal_test))
metal.sarima = forecast(fit.sarima, h=length(tsmetal_test)) 

checkresiduals(metal.naive)
checkresiduals(metal.ave)
checkresiduals(metal.drift)
checkresiduals(metal.sma)
checkresiduals(metal.ses)
checkresiduals(metal.ets)
checkresiduals(metal.arima)
checkresiduals(metal.sarima)


accuracy(metal.naive$mean, tsmetal_test)
accuracy(metal.ave$mean, tsmetal_test)
accuracy(metal.drift$mean, tsmetal_test)
accuracy(metal.sma$forecast, tsmetal_test)
accuracy(metal.ses$mean, tsmetal_test)
accuracy(metal.ets$mean, tsmetal_test)
accuracy(metal.arima$mean,tsmetal_test)
accuracy(metal.sarima$mean, tsmetal_test)

autoplot(window(tsmetal, start = c(2017.6))) +
  autolayer(metal.naive$mean, series="Naive") +
  autolayer(metal.ave$mean, series="Average") +
  autolayer(metal.drift$mean, series="Drift") +
  autolayer(metal.sma$forecast, series="SMA") +
  autolayer(metal.ses$mean, series="SES") +
  autolayer(metal.ets$mean, series="ETS") +
  autolayer(metal.arima$mean, series="ARIMA") +
  autolayer(metal.sarima$mean, series="SARIMA")
