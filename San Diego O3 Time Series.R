# San Diego Time Series Pollution Data

library("fpp2")
library("dplyr")
library("GGally")

ggpairs(as.data.frame(SanDiego.ts[,3:5]))


setwd("C:/Users/bsmith11/OneDrive/Documents/Babson/Time Series and Forecasting/Project")
SanDiego <- read.csv("San Diego Data.csv")

ggAcf(SanDiego$O3.Mean,lag=366) # Trend (I don't think there's seasonality)

SanDiego.ts <- ts(SanDiego$O3.Mean, start=c(2005,297), frequency=365)
SanDiego.ts
autoplot(SanDiego.ts)
str(SanDiego)

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
exhaustiveNon <- auto.arima(training,seasonal=FALSE ,stepwise=FALSE,approximation=FALSE) #ARIMA with exhaustive search
exhaustiveNon # ARIMA(0,1,3)
roughNon <- auto.arima(training,seasonal=FALSE)  #ARIMA with rough search
roughNon # ARIMA(1,1,2)
#exhaustiveSeasonal <- auto.arima(training,stepwise=FALSE,approximation=FALSE) #ARIMA with exhaustive search
#roughSeasonal <- auto.arima(training)  #ARIMA with rough search


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
checkresiduals(exhaustiveNon) # Not adequate (not white noise)
checkresiduals(roughNon) # Not adequate (not white noise)
# checkresiduals(exhaustiveSeasonal) 
# checkresiduals(roughSeasonal)

# Forecast on test for ARIMA
exhaustiveNonModel <- forecast(exhaustiveNon, length(test))
exhaustiveNonModel
roughNonModel <- forecast(roughNon, length(test))
roughNonModel

# exhaustiveSeasonalModel <- forecast(exhaustiveSeasonal, length(test))
# exhaustiveSeasonalModel
# roughSeasonalModel <- forecast(roughSeasonal, length(test))
# roughSeasonalModel



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
accuracy(exhaustiveNonModel,test)[2,] # MAPE: 32.398; RMSE: 0.142
accuracy(roughNonModel,test)[2,] # MAPE: 32.819; RMSE: 0.014
accuracy(exhaustiveSeasonalModel,test)[2,]
accuracy(roughSeasonalModel,test)[2,]

prednaive <- naive(SanDiego.ts,135)
preddrift <- rwf(SanDiego.ts,135)
predsn <- snaive(SanDiego.ts,135)
predavg <- meanf(SanDiego.ts,135)
predSES <- ses(SanDiego.ts,135)
predSES0.1 <- ses(SanDiego.ts,alpha = 0.1, 135)
predSES0.3 <- ses(SanDiego.ts,alpha = 0.3, 135)
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

autoplot(prednaive)
  + autolayer(fitted(predS), series = "Fitted")


#####
# Regression

# Selecting the 4 means of the different chemicals from the data set
SanDiegoMeans <- SanDiego %>%
  select(NO2.Mean, O3.Mean, SO2.Mean, CO.Mean)

tail(SanDiegoMeans)

SanDiegoMeans.ts <- ts(SanDiegoMeans, start=c(2005,297), frequency=365)
SanDiegoMeans.ts

# Scatterplot matrix
ggpairs(as.data.frame(SanDiegoMeans.ts))

# Displaying correlograms
ggAcf(SanDiegoMeans.ts[,1], lag = 365) # NO2: Cycle
ggAcf(SanDiegoMeans.ts[,2], lag = 365) # O3: Cycle
ggAcf(SanDiegoMeans.ts[,3], lag = 365) # SO2: Trend (Maybe seasonality at frequency = 12)
ggAcf(SanDiegoMeans.ts[,4], lag = 365) # CO: Cycle

SanDiegoMeansLagged <- cbind(O3 = SanDiegoMeans.ts[,"O3.Mean"],
                        NO2.Lag = stats::lag(SanDiegoMeans.ts[,"NO2.Mean"],-1),
                        SO2.Lag = stats::lag(SanDiegoMeans.ts[,"SO2.Mean"],-1),
                        CO.Lag = stats::lag(SanDiegoMeans.ts[,"CO.Mean"],-1))

# Delete values with NA
SanDiegoMeansLagged <- na.omit(SanDiegoMeansLagged)

# Step 1. Partitioning
testLag <- tail(SanDiegoMeansLagged, max(nrow(SanDiegoMeansLagged)*0.20), 135)
testLag

trainingLag <- head(SanDiegoMeansLagged, nrow(SanDiegoMeansLagged) - nrow(testLag))
trainingLag

## Step 2: fit models on training

Model_lag <- tslm(O3 ~ NO2.Lag + SO2.Lag + CO.Lag, trainingLag)
# no reason to add trend or season

# If Step 1 says not spurious, a small p-value (≤ 0.05) in 
# checkresiduals() results indicates that Yis not well explained by 
# the X’s. The model is not spurious but could be improved by adding 
# or dropping X’s

summary(Model_lag)
checkresiduals(Model_lag) # Spurious; p-value is less than 0.05, not well explained
# not adequate

## Step 3. predict on validation
Pred_lag <- forecast(Model_lag,data.frame(testLag)) 
Pred_lag

##  Step 4. performance measurement
accuracy(Pred_lag,testLag[,"O3"])[2,] # MAPE: 25.2675

# Prediction
New <- data.frame(NO2.Lag = c(7.956522), 
                  SO2.Lag = c(3.913043),
                  CO.Lag = c(.204348))

predictionsReg1Period <- forecast(Model_lag,New) 
predictions

