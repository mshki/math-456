# 1 Import Libraries
library(quantmod) # for quantitative financial modeling and testing
library(lmtest) # For diagnostic checking in linear regression models
library(dplyr) # For wrking with data frame 
library(PerformanceAnalytics) # For portfolio performance and analysis
library(ggplot2) # For plots
library(xts) # For extensible time series
library(tidyverse) # For data visualization 
library("feasts") # For "Feature Extraction And Statistics for Time Series" 
library("fable") # FOr commonly used time series forecasting models
library("lubridate") # For working with date-times and time-spans
library("gridExtra") # For grids
library(tseries) # For  time series analysis and computational finance
library(forecast) # For analyzing forecasts 
library(rugarch) # For univariate GARCH models

# 2 Data Exploration
getSymbols("CAT", src="yahoo", periodicity = "daily", from = "1986-03-13", to = "2021-05-31")
head(CAT)

colSums(is.na(CAT)) 

data <- cbind(
  Price = MSFT$MSFT.Close,
  Return=CalculateReturns(CAT$CAT.Close, method = 'log')) 
colnames(data) <- c('Price','Return')
head(data)

# 3 Verify Assumptions
plot(na.omit(data$Price), ylab='CAT Closing Price',main='Caterpillar Stock Price from 1986-2021',col='pink')
# Observe: The price increases over time despite showing volatility 
#          Mean and variance change over time.
#          Thus, stock price is NOT stationary over time

plot(na.omit(data$Return),main='Return of CAT')
# Observe: The stock's returns' mean and variance are constant overtime 
#          Thus, stock returns are stationary over time

# 3.1 Augmented Dickey Fuller Test 
#     Checking whether Price is Stationary 
adf.test(na.omit(data$Price))
# Observe: p-value = 0.99 > alpha = 0.05
#          We fail to reject null hypothesis 
#          Thus, Price of stock is not stationary

#      Checking whether Return is Stationary 
adf.test(na.omit(data$Return))
# Observe: p-value = 0.01 < alpha = 0.05
#          We reject null hypothesis 
#          Thus, Return of stock is stationary




