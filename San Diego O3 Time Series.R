# San Diego Time Series Pollution Data

library("fpp2")
setwd("C:/Users/bsmith11/OneDrive/Documents/Babson/Time Series and Forecasting/Project")
SanDiego <- read.csv("San Diego Data.csv")

ggAcf(SanDiego$O3.Mean,lag=100) # Trend (I don't think there's seasonality)

SanDiego.ts <- ts(SanDiego$O3.Mean, start=c(2005,297), frequency=365)
SanDiego.ts
autoplot(SanDiego.ts)

anyNA(SanDiego.ts) # False

#step 1
test <- tail(SanDiego.ts, max(length(SanDiego.ts)*.2, 135))
training <- head(SanDiego.ts,length(SanDiego.ts)-length(test))

# step 2 predict on test and step 3 adequacy

naive <- naive(training,length(test))
drift <- rwf(training,length(test), drift=TRUE)
sn <- snaive(training,length(test))
avg <- meanf(training,length(test))
SES <- ses(training,length(test)) #with optimal alphas
SES0.1 <- ses(training,alpha=.1,length(test))
SES0.3 <- ses(training,alpha=.3,length(test))
hw <- holt(training,length(test))
#hwm <- hw(training,seasonal="multiplicative",length(test)) # not seasonal so multiplicative will not work
#hwa <- hw(training,seasonal="additive",length(test)) # not seasonal so additive will not work
exsearchnon <- auto.arima(training,seasonal=FALSE ,stepwise=FALSE,approximation=FALSE) #ARIMA with exhaustive search
exsearchnon # ARIMA(0,1,3)
roughsearchnon <- auto.arima(training,seasonal=FALSE)  #ARIMA with rough search
roughsearchnon # ARIMA(1,1,2)
#exsearch <- auto.arima(training,stepwise=FALSE,approximation=FALSE) #ARIMA with exhaustive search
#roughsearch <- auto.arima(training)  #ARIMA with rough search


checkresiduals(naive) # Not adequate (not white noise)
checkresiduals(drift) # Not adequate (not white noise)
checkresiduals(sn) # Not adequate (not white noise)
checkresiduals(avg) # Not adequate (not white noise & pattern in residuals)
checkresiduals(SES) # Not adequate (not white noise)
checkresiduals(SES0.1) # Not adequate (not white noise)
checkresiduals(SES0.3) # Not adequate (not white noise)
checkresiduals(hw) # Not adequate (not white noise)
#checkresiduals(hwm)
#checkresiduals(hwa)
checkresiduals(exsearchnon) # Not adequate (not white noise)
checkresiduals(roughsearchnon) # Not adequate (not white noise)
#checkresiduals(modelexsearch) 
#checkresiduals(modelroughsearch)

# Forecast on test for ARIMA
exsearchnonModelTest <- forecast(exsearchnon, length(test))
exsearchnonModelTest
roughsearchnonModelTest <- forecast(roughsearchnon, length(test))
exsearchnonModelTest


accuracy(naive,test)[2,] # MAPE: 41.504; RMSE: 0.017
accuracy(drift,test)[2,] # MAPE: 46.122; RMSE: 0.019
accuracy(sn,test)[2,] # MAPE: 22.316; RMSE: 0.009
accuracy(avg,test)[2,] # MAPE: 31.85; RMSE: 0.014
accuracy(SES,test)[2,] # MAPE: 43.609; RMSE: 0.018
accuracy(SES0.1,test)[2,] # MAPE: 32.860; RMSE: 0.014
accuracy(SES0.3,test)[2,] # MAPE: 36.309; 0.0156
accuracy(hw,test)[2,] # MAPE: 47.976; RMSE: 0.020
#accuracy(hwa,test)[2,]
#accuracy(hwm,test)[2,]
accuracy(exsearchnonModelTest,test)[2,] # MAPE: 32.398; RMSE: 0.142
accuracy(roughsearchnonModelTest,test)[2,] # MAPE: 32.819; RMSE: 0.014
#accuracy(exsearch,test)[2,]
#accuracy(roughsearch,test)[2,]

prednaive <- naive(SanDiego.ts,135)
preddrift <- rwf(SanDiego.ts,135)
predsn <- snaive(SanDiego.ts,135)
predavg <- meanf(SanDiego.ts,135)
predSEs <- ses(SanDiego.ts,135)
predSES0.1 <- ses(SanDiego.ts,135)
predSES0.3 <- ses(SanDiego.ts,135)
predhw <- holt(SanDiego.ts,135)
#predhwa <- forecast(SanDiego.ts,135)
#predhwm <- forecast(SanDiego.ts,135)

exsearchnonModel <- Arima(SanDiego.ts, order = c(0,1,3))
roughsearchnonModel <- Arima(SanDiego.ts, order = c(1,1,2))

predexsearchnon <- forecast(exsearchnonModel,135)
predroughsearchnon <- forecast(roughsearchnonModel,135)
#predexsearch <- forecast(SanDiego.ts,135)
#predroughsearch <- forecast(SanDiego.ts,135)

# Graph of best MAPE (Seasonal naive)

autoplot(predsn) +
  # Red curve: fitted data from model
  autolayer(fitted(predsn), series = "Fitted")