library(forecast)
library(fracdiff)
library(lmtest)

# ARMA innovation function given with the homework assignment
arma.innovation <- function(x, arma.model, ar.truncation=10) {
  p <- arma.model$arma[1]
  q <- arma.model$arma[2]
  ar.coef <- arma.model$coef[seq_len(p)]
  ma.coef <- arma.model$coef[p + seq_len(q)]
  if (q == 0) {
    infinite.ar.coef <- ar.coef
  } else {
    infinite.ar.coef <- -ARMAtoMA(-ma.coef, -ar.coef, ar.truncation)
  }
  return(as.vector(filter(x, c(1, -infinite.ar.coef), side=1)))
}



# Combines linear model with the given ARIMA model
combineLinearARIMA <- function(residualData, ARIMAmodel,
                               x=gasData$inputGas, y=gasData$outputCO2,
                               adjustmentFunction=arma.innovation) {
  tempAdjustedInputData <- adjustmentFunction(x, ARIMAmodel)
  tempAdjustedOutputData <- adjustmentFunction(y, ARIMAmodel)
  
  templmARIMAmodel <- lm(tempAdjustedOutputData~tempAdjustedInputData)

  return(templmARIMAmodel)
}


# Plot the residuals data
plotResiduals <- function(modelARIMA) {
  tempResiduals = resid(modelARIMA)
  tempTSResiduals = ts(tempResiduals)
  plot(tempTSResiduals, type='l')
  acf(tempTSResiduals, 100)
  pacf(tempTSResiduals, 100)
}



# read gas data into a data frame
gasData <- read.csv('gas-furnace.csv')
# rename the columns
names(gasData) <- c('inputGas', 'outputCO2')
# first five observations
head(gasData)


##USE LINEAR REGRESSION MODEL AND PLOT THE ACF FUNCTION
gasData.lm <- lm(outputCO2~inputGas, data=gasData)
#generate residuals data for ARIMA models
gasData.r <- resid(gasData.lm)
# plot acf, and pacf functions
plotResiduals(gasData.lm)


##USE ARIMA(0,0,1) MODEL FOR THE RESIDUALS AND PLOT THE RESIDUALS
#set up for the ARIMA(0,0,1) model
ARIMA_0_0_1 <- Arima(gasData.r, order=c(0,0,1))
#combine linear model with the ARIMA model
gasData_lm_ARIMA_0_0_1 <- combineLinearARIMA(gasData.r, ARIMA_0_0_1)
# plot residuals, acf, and pacf functions
plotResiduals(gasData_lm_ARIMA_0_0_1)


##USE ARIMA(1,0,0) MODEL FOR THE RESIDUALS AND PLOT THE RESIDUALS
#set up for the ARIMA(1,0,0) model
ARIMA_1_0_0 <- Arima(gasData.r, order=c(1,0,0))
#combine linear model with the ARIMA model
gasData_lm_ARIMA_1_0_0 <- combineLinearARIMA(gasData.r, ARIMA_1_0_0)
#plot residuals, acf and pacf functions
plotResiduals(gasData_lm_ARIMA_1_0_0)


##USE ARIMA(0,0,2) MODEL FOR THE RESIDUALS AND PLOT THE RESIDUALS
ARIMA_0_0_2 <- Arima(gasData.r, order=c(0,0,2))
#combine linear model with the ARIMA model
gasData_lm_ARIMA_0_0_2 <- combineLinearARIMA(gasData.r, ARIMA_0_0_2)
#plot residuals, acf and pacf functions
plotResiduals(gasData_lm_ARIMA_0_0_2)


#USE ARIMA(2,0,0) MODEL FOR THE RESIDUALS AND PLOT THE RESIDUALS
ARIMA_2_0_0 <- Arima(gasData.r, order=c(2,0,0))
#combine linear model with the ARIMA model
gasData_lm_ARIMA_2_0_0 <- combineLinearARIMA(gasData.r, ARIMA_2_0_0)
#plot residuals, acf and pacf functions
plotResiduals(gasData_lm_ARIMA_2_0_0)


##USE ARIMA(2,0,2) MODEL FOR THE RESIDUALS AND PLOT THE RESIDUALS
ARIMA_2_0_2 <- Arima(gasData.r, order=c(2,0,2))
#combine linear model with the ARIMA model
gasData_lm_ARIMA_2_0_2 <- combineLinearARIMA(gasData.r, ARIMA_2_0_2)
#plot residuals, acf and pacf functions
plotResiduals(gasData_lm_ARIMA_2_0_2)


# ARFIMA model
#first model with calculating d manually and applying auto.arima
#get the fractional d
d <- fracdiff::fracdiff(gasData$outputCO2)
#do the fractional difference
st <- fracdiff::diffseries(gasData$outputCO2, d$d)
modelARFIMA_01 <- auto.arima(st)
#AIC of the first model
AIC(modelARFIMA_01)
#calculate and plot the residuals
plotResiduals(modelARFIMA_01)

#second model with calculating d and applying ARIMA in a single step
modelARFIMA_02 <- forecast::arfima(gasData$outputCO2)
#ACI of the second model
AIC(modelARFIMA_02)
#calculate and plot the residuals
plotResiduals(modelARFIMA_02)


##PERFORM SUMMARIES, DURBIN-WATSON, AND BOX-LJUNG TEST FOR EACH MODEL
##summary of the models
summary(ARIMA_0_0_1)
summary(ARIMA_1_0_0)
summary(ARIMA_0_0_2)
summary(ARIMA_2_0_0)
summary(ARIMA_2_0_2)
summary(modelARFIMA_02)
print(paste('AIC of ARFIMA Model: ', AIC(modelARFIMA_02)))
print(paste('BIC of ARFIMA Model: ', BIC(modelARFIMA_02)))

#Durbin-Watson and Ljung-Box tests
dwtest(resid(gasData_lm_ARIMA_0_0_1)~1, alternative='two.sided')
dwtest(resid(gasData_lm_ARIMA_1_0_0)~1, alternative='two.sided')
dwtest(resid(gasData_lm_ARIMA_0_0_2)~1, alternative='two.sided')
dwtest(resid(gasData_lm_ARIMA_2_0_0)~1, alternative='two.sided')
dwtest(resid(gasData_lm_ARIMA_2_0_2)~1, alternative='two.sided')
dwtest(resid(modelARFIMA_02)~1, alternative='two.sided')

#Ljung-Box tests
#According to the textbook (Hyndman) the h value should be 10 for non-seasonal
#data and h=2m for seasonal data, where m is the number of periods. On the other
#hand, it says the test is not good when h is larger. Even though, there is no
#time stamps assigned to the data points, I will assume that it is annual data,
#and will use h=12 to prevent h from getting too large.
Box.test(resid(gasData_lm_ARIMA_0_0_1), lag=12, type='Ljung-Box')
Box.test(resid(gasData_lm_ARIMA_1_0_0), lag=12, type='Ljung-Box')
Box.test(resid(gasData_lm_ARIMA_0_0_2), lag=12, type='Ljung-Box')
Box.test(resid(gasData_lm_ARIMA_2_0_0), lag=12, type='Ljung-Box')
Box.test(resid(gasData_lm_ARIMA_2_0_2), lag=12, type='Ljung-Box')
Box.test(resid(modelARFIMA_02), lag=12, type='Ljung-Box')


##BOOTSTRAP THE RESIDUALS DATA AND PLOT THE DISTRIBUTION OF THE COEFFICIENTS
numberOfBootstraps <- 500
listOfBoostrappedSeries <- bld.mbb.bootstrap(gasData.r, num=numberOfBootstraps)

# containers for the coefficients
coefficientsAR <- c()
coefficientsMA <- c()

for (i in seq(numberOfBootstraps)) {
  # convert list into an array
  tsData <- unlist(listOfBoostrappedSeries[i], use.names=F)
  # perform ARIMA model for each bootstrapped time-series
  tempARIMAmodel <- Arima(tsData, order=c(1,0,1))
  # collect the AR and MA coefficients
  coeffs <- tempARIMAmodel$coef
  coeffAR <- coeffs[1]
  coeffMA <- coeffs[2]
  # append the coefficients to their corresponding arrays
  coefficientsAR <- append(coefficientsAR, as.numeric(unlist(coeffAR, use.names=F)))
  coefficientsMA <- append(coefficientsMA, as.numeric(unlist(coeffMA, use.names=F)))
}

hist(coefficientsAR)
hist(coefficientsMA)


calculateCI <- function(dataArray, alpha) {
  # the mean of the data
  meanData <- mean(dataArray)
  
  # calculate the SE of the mean  
  N <- length(dataArray)
  std <- sd(dataArray)
  SE <- std/sqrt(N)
  
  # find the t-score
  alpha = alpha
  df = N - 1
  tScore = qt(p=alpha/2, df=df,lower.tail=F)
  
  # calculate the margin of error
  moe <- tScore * SE
  
  lowerBound <- meanData - moe
  upperBound <- meanData + moe
  
  return(c(lowerBound, upperBound))
}

ci95ARcoeffs <- calculateCI(coefficientsAR, 0.05)
print(paste('95% CI for the AR coefficients: (', ci95ARcoeffs[1], ci95ARcoeffs[2], ')'))

ci95MAcoeffs <- calculateCI(coefficientsMA, 0.05)
print(paste('95% CI for the MA coefficients: (', ci95MAcoeffs[1], ci95MAcoeffs[2], ')'))
