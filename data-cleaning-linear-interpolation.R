#install.packages("imputeTS")

library(imputeTS)
library(zoo)
library(xts)

# read and expand the time series by filling the missing date
read_and_expand_data <- function(filename) {
  data <- read.csv(filename)
  # generate yearmon index by combining year and month data
  data$index <- as.Date(data$DateOfSale, '%m/%d/%Y')
  data$index <- as.yearmon(data$index)
  # generate a series with no missing dates
  index <- seq.Date(as.Date('1-1-2001', '%m-%d-%Y'),
                    as.Date('12-31-2013', '%m-%d-%Y'), by='month')
  df_full_index <- as.data.frame(index)
  df_full_index$index <- as.yearmon(df_full_index$index)
  # merge the data with the generated series to fill the missing dates
  data_with_na <- merge(as.data.frame(data), df_full_index, by='index', all=TRUE)
  data_with_na <- data_with_na[, c('index', 'price')]
  return (data_with_na)
}

filename1 <- 'Documentation/data/commodity-prices/cmeS.csv'
filename2 <- 'Documentation/data/commodity-prices/immS.csv'
filename3 <- 'Documentation/data/commodity-prices/iomS.csv'

# read and process the data
cmePrices <- read_and_expand_data(filename1)
immPrices <- read_and_expand_data(filename2)
iomPrices <- read_and_expand_data(filename3)


# plot the missing values
ggplot_na_distribution(as.xts(cmePrices$price, cmePrices$index),
                       title='CME Seat Prices Missing Values')
ggplot_na_distribution(as.xts(immPrices$price, immPrices$index),
                       title='IMM Seat Prices Missing Values')
ggplot_na_distribution(as.xts(iomPrices$price, iomPrices$index),
                       title='IOM Seat Prices Missing Values')


# stats about the missing values
statsNA(as.xts(cmePrices$price, cmePrices$index))
statsNA(as.xts(immPrices$price, immPrices$index))
statsNA(as.xts(iomPrices$price, iomPrices$index))


# at this point, there are two problems with time-series data:
# 1. imputing missing values
# 2. compressing repeated periods
# For the first problem, we can use linear interpolation in order to keep connect
# the dots linearly. This approach will prevent the distribution have fluctuations
# while imputing the missing values.
# For the second problem, we can either go with the mean and the median. However,
# taking mean would be easier since the number of repeated period varies significantly.
# As a summary, we will use linear interpolation for imputation and mean aggregation
# for compressing the data.

# write a function which interpolates the missing values, compresses the
# repeated values and plots the final result while returning the time-series
# under normal conditions, we have to decompose the function into several
# functions and let each function to perform one duty at a time. However,
# this will help us to efficiently provide the required parts of the assignment.

interpolate_and_plot <- function(data, title) {
  # get the row numbers of the missing values
  idx_missing <- which(is.na(data$price))
  # get the row numbers of the repeated values
  data_aggreagated <- aggregate(data[, c('price')], list(data$index), length)
  idx_repeated <- which(data_aggreagated$x > 1)
  
  # interpolate the missing values
  data$price <- na_interpolation(data$price)
  # compress repeated values
  data <- aggregate(data[, c('price')], list(data$index), mean)
  # rename the columns
  names(data) <- c('index', 'price')

  plot(data$index, data$price, pch=1, main=title, type='l',
       xlab='Price', ylab='Year')
  points(data$index[idx_missing], data$price[idx_missing], pch=17, col='red')
  points(data$index[idx_repeated], data$price[idx_repeated], pch=19, col='blue')
  legend('topleft', 
         legend=c('Linear Interpolation', 'Mean Compressed'),
         col=c('red', 'blue'), pch=c(17, 19))

  return(data)  
}


cme_clean <- interpolate_and_plot(cmePrices, 'CME Seat Prices Original and Interpolated Data')
imm_clean <- interpolate_and_plot(immPrices, 'IMM Seat Prices Original and Interpolated Data')
iom_clean <- interpolate_and_plot(iomPrices, 'IOM Seat Prices Original and Interpolated Data')