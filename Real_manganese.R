
install.packages("gridExtra")
install.packages("smooth")
install.packages("fpp2")
install.packages("stats")
install.packages("opera")

library(gridExtra)
library(smooth)
library(fpp2)
library(stats)
library(opera)
library(faraway)
#-plot & decomposing

metal = read.csv("Manganese.csv")
tssimetal = ts(metal$Mn44,start=c(2006,6),end = c(2018,12), freq = 12)
tssimetal
plot(tssimetal, xlab="Time", ylab="Price of SiMn", main="Monthly SiMn Price")
autoplot(tssimetal) +
  ggtitle("Monthly SiMn Price") +
  xlab("Time") +
  ylab("Price of SiMn")

ggseasonplot(tssimetal, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price of SiMn") +
  ggtitle("Seasonal plot for SiMn price")

ggseasonplot(tssimetal, polar=TRUE) +
  ylab("Price of SiMn") +
  ggtitle("Seasonal plot for SiMn price")

ggsubseriesplot(tssimetal) +
  ylab("Price of SiMn") +
  ggtitle("Seasonal plot for SiMn price")

add.decomp = decompose(tssimetal, "additive")
autoplot(add.decomp)

#-Auto correlation

ggAcf(tssimetal, plot=FALSE)

ggAcf(tssimetal)

ggAcf(tssimetal, lag.max=48)


#- White noise

res = residuals(naive(tssimetal))
res
autoplot(res)
ggAcf(res)
checkresiduals(naive(res))

ggPacf(tssimetal, lag.max=48)

ggPacf(tssimetal, lag.max=100)

ggPacf(tssimetal, lag.max=48)


#-Forecasting

tsmetal_train = window(tssimetal, start = c(2006,6), end = c(2017,12))
tsmetal_test = window(tssimetal, start = c(2018,1), end = c(2018,12))

fit.ets = ets(tsmetal_train)
fit.arima = auto.arima(tsmetal_train, seasonal=FALSE)
fit.sarima = auto.arima(tsmetal_train)


summary(fit.arima)
summary(fit.sarima)


metal.naive = naive(tsmetal_train, h=12)
metal.ave   = meanf(tsmetal_train, h=12) 
metal.drift = rwf(tsmetal_train, drift=TRUE, h=12)
metal.sma   = sma(tsmetal_train, h=12)
metal.ses   = ses(tsmetal_train, h=12)
metal.ets = forecast(fit.ets, h=12)
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

autoplot(window(tssimetal, start = c(2018.1))) +
  autolayer(metal.naive$mean, series="Naive") +
  autolayer(metal.ave$mean, series="Average") +
  autolayer(metal.drift$mean, series="Drift") +
  autolayer(metal.sma$forecast, series="SMA") +
  autolayer(metal.ses$mean, series="SES") +
  autolayer(metal.ets$mean, series="ETS") +
  autolayer(metal.arima$mean, series="ARIMA") +
  autolayer(metal.sarima$mean, series="SARIMA")

#- combined model


autoplot(tssimetal) +
  ylab("$ Mn44")


fit1 = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=1), seasonal=FALSE)
fc1  = forecast(fit1, xreg = fourier(tsmetal_train, K=1, h = length(tsmetal_test)))


fit2 = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=2), seasonal=FALSE)
fc2  = forecast(fit2, xreg = fourier(tsmetal_train, K=2, h = length(tsmetal_test)))


fit3 = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=3), seasonal=FALSE)
fc3  = forecast(fit3, xreg = fourier(tsmetal_train, K=3, h = length(tsmetal_test)))


fit4 = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=4), seasonal=FALSE)
fc4  = forecast(fit4, xreg = fourier(tsmetal_train, K=4, h = length(tsmetal_test)))


fit5 = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=5), seasonal=FALSE)
fc5  = forecast(fit5, xreg = fourier(tsmetal_train, K=5, h = length(tsmetal_test)))


fit6 = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=6), seasonal=FALSE)
fc6  = forecast(fit6, xreg = fourier(tsmetal_train, K=6, h = length(tsmetal_test)))


p1 = autoplot(fc1) +
       ggtitle("K=1") +
       ylab("")
       
       p2 = autoplot(fc2) +
       ggtitle("K=2")  +
       ylab("")
       
       p3 = autoplot(fc3) +
       ggtitle("K=3")  +
       ylab("")
       
       p4 = autoplot(fc4) +
       ggtitle("K=4") +
       ylab("")
       
       p5 = autoplot(fc5) +
       ggtitle("K=5")  +
       ylab("")
       
       p6 = autoplot(fc6) +
       ggtitle("K=6")  +
       ylab("")
       
       
       grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 3, ncol = 2)
       
       grid.arrange(p1,p2,p4,p6, nrow = 2, ncol = 2)
 
fit1$aicc
fit2$aicc
fit3$aicc
fit4$aicc
fit5$aicc
fit6$aicc


fit  = list()
fc   = list()
aicc = list()
p    = list()

for (i in seq(6)) {
  fit[[i]] = auto.arima(tsmetal_train, xreg = fourier(tsmetal_train, K=i), seasonal=FALSE)
  fc[[i]] = forecast(fit[[i]], xreg = fourier(tsmetal_train, K=i, h = length(tsmetal_test)))
  aicc[[i]] = fit[[i]]$aicc
  
  p[[i]] = autoplot(fc[[i]]) +
    ylim(0,20) +
    xlim(2006,2019) +
    ggtitle(paste("K =",i))  +
    ylab("")
}

aicc

autoplot(ts(aicc))

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], nrow = 3, ncol = 2)



fc.ets = forecast(tsmetal_train, h = length(tsmetal_test))
fc.arima = forecast(auto.arima(tsmetal_train, seasonal=FALSE), h = length(tsmetal_test))
fc.sarima = forecast(auto.arima(tsmetal_train), h = length(tsmetal_test))

accuracy(fc.ets$mean, tsmetal_test)
accuracy(fc.arima$mean, tsmetal_test)
accuracy(fc[[1]]$mean, tsmetal_test)
accuracy(fc.sarima$mean, tsmetal_test)

X = cbind(ETS = fc.ets$mean, ARIMA = fc.arima$mean, SARIMA = fc.sarima$mean, FOURIER = fc[[1]]$mean)

mixt1 = mixture(model = "MLpol", loss.type = "square")
mixt2 = mixture(model = "OGD", loss.type = "square")

weights1 = predict(mixt1, X, tsmetal_test, type="weights")
weights2 = predict(mixt2, X, tsmetal_test, type="weights")

weights1

weights2

response1 = predict(mixt1, X, tsmetal_test, type="response")
response2 = predict(mixt2, X, tsmetal_test, type="response")

accuracy(fc.ets$mean, tsmetal_test)
accuracy(fc.arima$mean, tsmetal_test)
accuracy(fc[[1]]$mean, tsmetal_test)
accuracy(fc.sarima$mean, tsmetal_test)
accuracy(as.numeric(response1), tsmetal_test)
accuracy(as.numeric(response2), tsmetal_test)

# Dynamic Regression

total = read.csv("totaldata3.csv")
tstotal =ts(total, start= c(2014,4), end=c(2018,12), freq =12)
autoplot(tstotal)
plot(tstotal)

reg= lm(Mn44 ~ iron+HC.FeMn+SiMn, data = total)
summary(reg)

reg2= lm(Mn44 ~ SiMn, data = total)
summary(reg2)


str(total)

autoplot(ts(total[,"Mn44"])) +
  xlab("month") +
  ylab("Price")

ggplot(total) +
  geom_point(aes(x=total[,"SiMn"], y=total[,"Mn44"])) +
  xlab("SiMn") +
  ylab("Mn44")

tstotal = ts(total,start=c(2014,4),end=c(2018,12), freq=12) #see the numbering R uses
tstotal
tstrain = window(tstotal, end = c(2017,12))
tstest  = window(tstotal, start = c(2018,1))

Mn44        = tstrain[,"Mn44"]
SiMn       = tstrain[,"SiMn"]

extvar = cbind(SiMn)

# extvar

dlr.fit = auto.arima(Mn44, xreg = extvar)

summary(dlr.fit)  

checkresiduals(dlr.fit)

extvar_test = tstest[,"SiMn"]


dlr.fc = forecast(dlr.fit, xreg = extvar_test)
dlr.fc
autoplot(dlr.fc)
accuracy(dlr.fc$mean, tstest[,"Mn44"])

autoplot(decompose(tstotal[,"Mn44"]))

# Exercise: Forecasting with other methods
total.naive  = naive(Mn44, h=nrow(tstest))
total.ave    = meanf(Mn44, h=nrow(tstest)) 
total.drift  = rwf(Mn44, drift=TRUE, h=nrow(tstest))
total.sma    = sma(Mn44, h=nrow(tstest))
total.ses    = ses(Mn44, h=nrow(tstest))
total.ets    = forecast(Mn44, h=nrow(tstest))
total.arma   = forecast(auto.arima(Mn44,d=0,seasonal=FALSE), h=nrow(tstest))
total.arima  = forecast(auto.arima(Mn44,seasonal=FALSE), h=nrow(tstest))
total.sarima = forecast(auto.arima(Mn44), h=nrow(tstest))

accuracy(total.naive$mean, tstest[,"Mn44"])
accuracy(total.ave$mean, tstest[,"Mn44"])
accuracy(total.drift$mean, tstest[,"Mn44"])
accuracy(total.sma$forecast, tstest[,"Mn44"])
accuracy(total.ses$mean, tstest[,"Mn44"])
accuracy(total.ets$mean, tstest[,"Mn44"])
accuracy(total.arma$mean, tstest[,"Mn44"])
accuracy(total.arima$mean, tstest[,"Mn44"])
accuracy(total.sarima$mean, tstest[,"Mn44"])

checkresiduals(total.sarima)

checkresiduals(dlr.fc)

autoplot(window(tstotal[,"Mn44"], start = c(2018,1))) +
  autolayer(total.ets$mean, series="ETS") +
  autolayer(total.sarima$mean, series="SARIMA") +
  autolayer(dlr.fc$mean, series="DLR")


# standard linear regression without ARIMA errors 

reg2= lm(Mn44 ~ HC.FeMn, data = total)
summary(reg2)


checkresiduals(reg2$residuals)

testData = data.frame(extvar_test)

reg.fc = predict(reg2, newdata = data.frame(HC.FeMn=testData[,1]))

accuracy(reg.fc, tstest[,"Mn44"])

total3 = read.csv("totaldata3.csv")
reg3 = lm(Mn44 ~ SiMn + HC.FeMn, data = total3)
summary(reg3)
round(cor(total3), digits=3)
vif(total3[,c(1,2,4)])
vif(total3[,c(2,4)])


#basic summary of SiMn

Simetal = read.csv("totaldata3.csv")
Simetal = Simetal[,-c(1,2,3)]
tssimetal = ts(Simetal,start=c(2014,4), freq = 12)
tssimetal

autoplot(tssimetal)

plot(tssimetal, xlab="Time", ylab="Price of SiMn", main="Monthly SiMn Price")
autoplot(tssimetal) +
  ggtitle("Monthly SiMn Price") +
  xlab("Time") +
  ylab("Price of SiMn")

ggseasonplot(tssimetal, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price of SiMn") +
  ggtitle("Seasonal plot for SiMn price")

ggseasonplot(tssimetal, polar=TRUE) +
  ylab("Price of SiMn") +
  ggtitle("Seasonal plot for SiMn price")

ggsubseriesplot(tssimetal) +
  ylab("Price of SiMn") +
  ggtitle("Seasonal plot for SiMn price")

add.decomp = decompose(tssimetal, "additive")
autoplot(add.decomp)
