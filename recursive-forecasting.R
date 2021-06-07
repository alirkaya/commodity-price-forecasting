#install.packages("TSA")
#install.packages("TSstudio")

library(TSA)
library(forecast)
library(zoo)
library(TSstudio)
library(lmtest)
library(stats)

# load beer sales data
data("beersales")

# plot the data
ts.plot(beersales)

# split the data into training and test sets
split_beersales <- ts_split(beersales, sample.out = 12)
train <- split_beersales$train
test <- split_beersales$test

# build an ARIMA model using auto.arima function
auto.arima(train)
suggested_ARIMA_model <- Arima(train, order=c(4,1,2),
                               seasonal=list(order=c(2,1,2), period=12))

# 1A forecast each month of 1990
recursive_1990_forecast <- forecast(suggested_ARIMA_model, h=12)

# 1B iterative forecasting
train_copy <- split_beersales$train

residuals <- c()
mean_residuals <- c()
p_values_autocorrelation <- c()
p_values_normality <- c()
forecasted_sales <- c()

for (i in seq(12)) {
  fit <- auto.arima(train_copy)
  
  # store the residuals
  residuals <- append(residuals, list(resid(fit)))
  
  # store mean of the residuals
  mean_residuals <- append(mean_residuals, mean(resid(fit)))
  
  # test for autocorrelation
  autocorrtest <- Box.test(resid(fit), lag=2, type='Ljung-Box')
  p_values_autocorrelation <- append(p_values_autocorrelation, autocorrtest$p.value)
  
  # test for normality
  normalitytest <- shapiro.test(resid(fit))
  p_values_normality <- append(p_values_normality, normalitytest$p.value)
  
  # forecast the next period
  forecasted_amount <- forecast(fit, h=1)
  forecasted_sales <- append(forecasted_sales, forecasted_amount$mean)
  
  # add forecasted amount to the training set
  train_copy <- ts(c(train_copy, forecasted_amount$mean), start=start(train_copy),
                   frequency=frequency(train_copy))

}

# 1C plot the mean, the p-value of the autocorrelation test and the p-value
# of the normality test of 12 models

plot(p_values_autocorrelation)
plot(p_values_normality)
plot(mean_residuals)

# 2 Plot the actual values with the recursive and direct recursive forecasts

idx_values <- seq(12)
plot(idx_values, test, type='o', col='blue', pch='o', lty=1, ylim=c(12.5, 17.5))
# add resursive forecasting
points(idx_values, recursive_1990_forecast$mean, col='red', pch='*')
lines(idx_values, recursive_1990_forecast$mean, col='red', lty=2, lwd=3)
# add direct recursive forecasting
points(idx_values, forecasted_sales, col='green', pch='+')
lines(idx_values, forecasted_sales, col='green', lty=3, lwd=2)
legend('topleft', legend=c('Actual Sales', 'Recursive Forecasts', 'DirRec Forecasts'),
       col=c('blue', 'red', 'green'), lty=1:3)

# Calculate MSE
mse_recursive_forecast <- sum((recursive_1990_forecast$mean - test) ** 2) / length(test)
print(paste('MSE of Recursive Forecast', mse_recursive_forecast))
mse_dirrec_forecast <- sum((forecasted_sales - test) ** 2) / length(test)
print(paste('MSE of DirRec Forecast', mse_dirrec_forecast))


