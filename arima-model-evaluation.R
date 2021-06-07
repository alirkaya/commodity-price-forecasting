require(forecast)
require(xts)
require(tseries)

riders <- read.csv("average-monthly-ridership.csv")

# use xts to divide data into train and test
riders["yearmon"] <- as.yearmon(riders$Month)
riders_xts <- xts(riders$average.monthly.bus.ridership.in.100, riders$yearmon)
riders_train <- window(riders_xts, end = as.yearmon("2018-12"))
riders_actual <- window(riders_xts, start=as.yearmon("2019-01"))

# EDA - data plot demonstrates it is non-stationary given the upward drift (aka non-deterministic trend)
# acf confirms - long drawn decay not immediate
plot(riders_train)
acf(riders_train)

# and the quantitative tests confirm non-stationarity
adf.test(riders_train)
kpss.test(riders_train)

num_of_months_forecast = 6
# baseline model using auto.arima with no seasonality
model1 <- auto.arima(riders_train, seasonal = FALSE)
summary(model1)
# d = 1, i.e. first different which is the correct way to convert drift TS to stationary TS

riders_est1 <- forecast(model1, num_of_months_forecast)
plot(riders_est1)
accuracy(riders_est1$mean, riders_actual[1:num_of_months_forecast])

# solution is expected to use Arima() and develop 2 additional model with diff orders
# given auto.arima model is (0,1,2), this solution tries 3 additional models
# with orders (0,1,1), (0,1,3) and (1,1,1) and no seasonality
model2 <- Arima(as.ts(riders_train), include.drift = TRUE, order = c(0,1,1))
summary(model2)
riders_est2 <- forecast(model2, num_of_months_forecast)
plot(riders_est2)
accuracy(riders_est2$mean, riders_actual[1:num_of_months_forecast])

model3 <- Arima(as.ts(riders_train), include.drift = TRUE, order = c(0,1,3))
summary(model3)
riders_est3 <- forecast(model3, num_of_months_forecast)
plot(riders_est3)
accuracy(riders_est3$mean, riders_actual[1:num_of_months_forecast])

model4 <- Arima(as.ts(riders_train), include.drift = TRUE, order = c(1,1,1))
summary(model4)
riders_est4 <- forecast(model4, num_of_months_forecast)
plot(riders_est4)
accuracy(riders_est4$mean, riders_actual[1:num_of_months_forecast])

# note model1 has the lowest RMSE for the 1969 test data
# and model3 has the lowest ACF1 for training data (autocorrelation of errors at lag1)

