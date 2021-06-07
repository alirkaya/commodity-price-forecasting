# install.packages("DataCombine")
library(tseries)
library(DataCombine)
library(xts)

fname = "Documentation/data/regression-data-akbilgic.csv"
df <- read.csv(fname)
print(head(df))

### Determine if all TS stationary - Qualitatively
# plot ISE
plot(df$ISE, type='l')
# plot SP
plot(df$SP, type='l')
# plot DAX
plot(df$DAX, type='l')
# plot FTSE
plot(df$FTSE, type='l')
# plot NIKKEI
plot(df$NIKKEI, type='l')
# plot BOVESPA
plot(df$BOVESPA, type='l')

### Determine if all TS stationary - Quantitatively
# ADF and KPSS tests
# ADF test -> H0: Non-Stationary; HA: Stationary
# KPSS test -> H0: Stationary; HA: Non-Stationary

adf.test(df$ISE)
kpss.test(df$ISE)

adf.test(df$SP)
kpss.test(df$SP)

adf.test(df$DAX)
kpss.test(df$DAX)

adf.test(df$FTSE)
kpss.test(df$FTSE)

adf.test(df$NIKKEI)
kpss.test(df$NIKKEI)

adf.test(df$BOVESPA)
kpss.test(df$BOVESPA)


### Split data into test and training sets
df$date = as.Date(df$date, '%d-%b-%y')
df_xts = as.xts(df[, c(2, 3, 4, 5, 6, 7)], order.by = df$date)

train = window(df_xts, end = as.Date('2011-02-8'))
test = window(df_xts, start = as.Date('2011-02-9'))

### Linearly regress ISE with other stock indexes
lm_base_model <- lm(ISE ~ SP + DAX + FTSE + NIKKEI + BOVESPA, data=train)
summary(lm_base_model)


### lag one day until all coefficients are significant at 0.02 level
# lag one time unit
df_train <- as.data.frame(train)
df_train <- slide(df_train, 'SP', NewVar = 'SP_lag_01', slideBy = -1)
df_train <- slide(df_train, 'BOVESPA', NewVar = 'BOVESPA_lag_01', slideBy = -1)

lm_lagged_01 <- lm(ISE ~ SP_lag_01 + DAX + FTSE + NIKKEI + BOVESPA_lag_01, data=df_train)
summary(lm_lagged_01)

# lag two times
df_train <- slide(df_train, 'SP', NewVar = 'SP_lag_02', slideBy = -2)

lm_lagged_02 <- lm(ISE ~ SP_lag_02 + DAX + FTSE + NIKKEI + BOVESPA_lag_01, data=df_train)
summary(lm_lagged_02)


### Find the correlation matrix
cor(df_train[, c(1, 2, 3, 4, 5, 6)])


### Take the test set, perform the same lags and call predict function
df_test <- as.data.frame(test)
head(df_test)

df_test <- slide(df_test, 'SP', NewVar = 'SP_lag_02', slideBy = -2)
df_test <- slide(df_test, 'BOVESPA', NewVar = 'BOVESPA_lag_01', slideBy = -1)

predictions <- predict(lm_lagged_02, newdata = df_test)