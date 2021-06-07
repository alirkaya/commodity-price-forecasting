library(readxl)
library(dplyr)
library(xts)
library(zoo)
library(tseries)
library(lubridate)
library(forecast)

##READ DATA INTO A DATA FRAME##

# get the file names
listOfFileNames <- dir(path='Documentation/data/traffic')
# add relative directory to each file name
for (i in seq(length(listOfFileNames))) {
  listOfFileNames[i] = paste('data/', listOfFileNames[i], sep='')
}


# the first file is July 1 and all other in chronological order
# let's order the list according to dates
idx <- seq(from=2, to=length(listOfFileNames))
idx[length(listOfFileNames)] = 1
listOfFileNames <- listOfFileNames[idx]


#reads excel files into a single data frame by appending
readDataFromExcel <- function(listOfFileNames) {
  
  dataFrames = c()
  days <- seq.Date(from = as.Date('2013-06-16'),
                   to = as.Date('2013-07-01'), by = 'days')
  
  for (idx in seq_along(listOfFileNames)) {
    temp = read_excel(listOfFileNames[idx], range=cell_cols('C:E'))
    temp <- temp[3:26,]
    temp <- temp[, c(1, 3)]
    temp$day <- days[idx]
    temp$index <- paste(temp$day, temp$Time, sep=' ')
    temp <- temp[c(4, 2)]
    names(temp)[2] <- "hourlyAverageCounts"
    dataFrames <- append(dataFrames, list(temp))
  }
  
  #put them all into a single dataframe
  df <- bind_rows(dataFrames)
  df$hourlyAverageCounts <- as.integer(df$hourlyAverageCounts)
  
  return(data.frame(df))
}


# generate data frame
df <- readDataFromExcel(listOfFileNames=listOfFileNames)


# split into train and test sets
df$index <- as.POSIXct(df$index)
df_ts <- xts(df$hourlyAverageCounts, df$index)

trainData <- window(df_ts, end=as.POSIXct('2013-06-30 23:00'))
actualData <- window(df_ts, start=as.POSIXct('2013-07-01 00:00'))

#EDA#
plot(trainData)
# test for stationarity
adf.test(trainData)
kpss.test(trainData)
# acf & pacf plots
acf(trainData)
pacf(trainData)

##Part1 - Use auto.arima()
##Fit Auto ARIMA model
auto.arima(as.ts(trainData))

## generate alternative models and compare AIC and BIC
Arima(as.ts(trainData), order = c(2,0,2))

Arima(as.ts(trainData), order = c(3,0,2))

Arima(as.ts(trainData), order = c(2,0,1))

##Part2 - Use day of the week model
## generate week of the day data
ts_weekly <- ts(trainData, frequency = 24 * 7)

auto.arima(ts_weekly)

##Part3 - Use hour of the day model
## generate hour of the day data
ts_hourly <- ts(trainData, frequency = 24)

auto.arima(ts_hourly)

##Part4 - Compare forecasting performances
## forecasting July 1 based on weekly and hourly ts data
modelWeekly <- Arima(as.ts(trainData), order = c(0,1,2),
                     seasonal = list(order=c(0,1,0), period=24 * 7))

modelHourly <- Arima(as.ts(trainData), order = c(2,0,1),
                     seasonal = list(order=c(2,1,0), period=24))


forecastWeekly <- forecast(modelWeekly, h=24)
forecastHourly <- forecast(modelHourly, h=24)

plot(forecastWeekly)
plot(forecastHourly)

accuracy(forecastWeekly, actualData, test=c(8,9,17,18))
accuracy(forecastHourly, actualData, test=c(8,9,17,18))