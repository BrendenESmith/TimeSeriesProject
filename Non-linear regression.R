library(fpp2)
library(dplyr)

setwd("C:/Users/bsmith11/OneDrive/Documents/Babson/Time Series and Forecasting/Project")
SanDiegoData <- read.csv("San Diego Data.csv")

SanDiegoMeans <- SanDiegoData %>%
  select(NO2.Mean, O3.Mean, SO2.Mean, CO.Mean)

SanDiegoMeans.ts <- ts(SanDiegoMeans, start=c(2005,297), frequency=365)

SanDiegoMeansLagged <- cbind(O3 = SanDiegoMeans.ts[,"O3.Mean"],
                             NO2.Lag = stats::lag(SanDiegoMeans.ts[,"NO2.Mean"],-1),
                             SO2.Lag = stats::lag(SanDiegoMeans.ts[,"SO2.Mean"],-1),
                             CO.Lag = stats::lag(SanDiegoMeans.ts[,"CO.Mean"],-1))

# Delete values with NA
SanDiegoMeansLagged <- na.omit(SanDiegoMeansLagged)
# 
# ggAcf(SanDiegoMeans$NO2.Mean) # White noise
# ggAcf(SanDiegoMeans$O3.Mean) # White noise

# plot(SanDiegoMeans$ï..Sales, SanDiegoMeans$Expend.)

LaggedMeans <- ts(SanDiegoMeansLagged[,c("O3","NO2.Lag", "SO2.Lag", "CO.Lag")])

##############

test <- tail(LaggedMeans, max(nrow(LaggedMeans)*0.2, 1))
test
training <- head(LaggedMeans, nrow(LaggedMeans)- nrow(test))

# 1/x transform
which(LaggedMeans[,c("NO2.Lag", "SO2.Lag", "CO.Lag")]==0) # no zero (can't have 0)
X_1 <- tslm(O3~ I(1/NO2.Lag) + I(1/SO2.Lag) + I(1/CO.Lag), training)
summary(X_1) # Not spurious, not well-explained
checkresiduals(X_1) # Not adequate

# log transformation
min(LaggedMeans[,c("NO2.Lag", "SO2.Lag", "CO.Lag")]) # All x values are positive (can't have negatives)

logx <- tslm(O3~ I(log(NO2.Lag)) + I(log(SO2.Lag)) + I(log(CO.Lag)), training)
summary(logx) # Not spurious, not well-explained
checkresiduals(logx) # Not adequate

# Square root transformation
# min(tsObject[ ,c("X ")]) # All x values must be equal or greater than 0

# slm(Y ~ I(sqrt(X)), training)

min(LaggedMeans[ ,c("NO2.Lag", "SO2.Lag", "CO.Lag")]) # no zeros
sqrt_x <- tslm(O3~ I(sqrt(NO2.Lag)) + I(sqrt(SO2.Lag)) + I(sqrt(CO.Lag)), training)
summary(sqrt_x) # Not spurious, not well-explained
checkresiduals(sqrt_x) # Not adequate

# Squared transformation
# No constraints on x values

# tslm(Y ~ I(X^2), training)

x_sq <- tslm(O3~ I(NO2.Lag^2) + I(SO2.Lag^2) + I(CO.Lag^2), training)
summary(x_sq) # Not spurious, not well-explained
checkresiduals(x_sq) # Not adequate



# Step 3. Predict on test set

pred_x_1 <- forecast(X_1, data.frame(test))
pred_logx <- forecast(logx, data.frame(test))
pred_sqrt_x <- forecast(sqrt_x, data.frame(test))
pred_x_sq <- forecast(x_sq, data.frame(test))

# Step 4. Accuracy
accuracy(pred_x_1, test[,"O3"])[2,] # MAPE: 31.478; RMSE: 0.0143
accuracy(pred_logx, test[,"O3"])[2,] # MAPE: 27.696; RMSE: 0.0130
accuracy(pred_sqrt_x, test[,"O3"])[2,] # MAPE: 26.199; RMSE: 0.01257
accuracy(pred_x_sq, test[,"O3"])[2,] # MAPE: 24.853; RMSE: 0.01234

#### Predict for 1 day

New <- data.frame(NO2.Lag = 7.956522, SO2.Lag = 3.913043, CO.Lag = .204348) # Next record

forecast(x_sq, New) # Forecast: 0.0300

##################################

# log-log transformation

# Converting Y to log(Y)
LaggedMeans[,"O3"] <- log(LaggedMeans[,"O3"]) 
LaggedMeans

# Partitioning
test_log <- tail(LaggedMeans, max(nrow(LaggedMeans)*0.2, 1)) 
training_log <- head(LaggedMeans,nrow(LaggedMeans) - nrow(test_log))
training_log

# Fit linear model on the converted Y, log(Y)
loglog <- tslm(O3 ~ NO2.Lag + SO2.Lag + CO.Lag, training_log)

summary(loglog) # Spurious, not well explained
checkresiduals(loglog) # Not adequate


Pred_loglog <- ts(c(t(exp(predict(loglog,test_log)))))
Pred_loglog

accuracy(Pred_loglog,ts(test[,"O3"])) # MAPE: 28.32053, RMSE: 0.01363669


###### predict for 1 period into the future

LaggedMeans # Show time stamps
New = data.frame(NO2.Lag = 7.956522, SO2.Lag = 3.913043, CO.Lag = .204348)

# 1-day Forecast
exp(data.frame(forecast(loglog, New))) # Forecast: 0.3443

