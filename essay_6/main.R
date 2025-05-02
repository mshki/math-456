# 0 Import Libraries
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

# 1 Get Data
getSymbols("CAT", src="yahoo", periodicity = "daily", from = "1986-03-13", to = "2021-05-31")
head(CAT)

# 2 Calculate (log) returns
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

# 3.1 Augmented Dickey Fuller Test (Stationary Test)
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

# 3.2 (Partial) Autocorrelation Function
#      Checking whether Return is Autocorrelated
acf(na.omit(data$Return), lag.max = 40, main='ACF of Return Values',col='pink')
pacf(na.omit(data$Return), main='Partial Auto Correlation of Return Values',col='pink')
# Observe: ACF shows Return series is not dependent on previous days error
#          PACF shows Return series autocorrelates with lags 
#          Thus, Return series are influenced by previous days Return

# 3.3 Normality Test 
ggplot(aes(Return), data=data) + geom_histogram(bins = 100,col='black',fill='pink') + ggtitle('Return of MSFt')
skewness(data$Return); kurtosis(data$Return)
# Observe: Returns are not normally distributed!
#          Negative skew indicates Leptokurtic distribution

skewness(data$Return); kurtosis(data$Return)
# Observe: QQ Plot Verifies distribution of Returns 

jarque.bera.test(na.omit(data$Return))
# Observe: Jarque Bera Test 
#          p-value < 2.2e-16 < alpha = 0.05
#          We reject null hypothesis 
#          Thus Return series is not normally distributed

# 3.4 Absence of Auto Correlation
Box.test(na.omit(data$Return), type = "Ljung-Box")
# Observe: Ljung-Box Test 
#          p-value < 0.05751 < alpha = 0.01
#          We reject null hypothesis 
#          Thus Return series is not independent

# 3.5 Autocorrelation of Absolute/Squaare Return
a<- ggAcf(abs(na.omit(data$Return)), col='pink',main='Acf of Absolute Return of MSFT')
p<- ggPacf(abs(na.omit(data$Return)),col='thistle1',main='PAcf of Absolute Return of MSFT')
grid.arrange(a,p, ncol = 2, nrow = 1)
# Observe: Absolute Return series show slow decay of auto correlation 

c <- ggAcf(na.omit(data$Return)^2, lag.max = 40, col='red', main='ACF of squared Return Values')
d<- ggPacf(na.omit(data$Return)^2,lag.max = 40, col='steelblue',main= 'PACF of squared Return Values')
grid.arrange(c,d, ncol = 2, nrow = 1)
# Observe: Squared Return series show slow decay of auto correlation 

# 3.6 Volatility Clustering 
chart.RollingPerformance(na.omit(data$Return),width = 22,FUN = 'sd.annualized',scale=252, main = 'Rolling 1 month Volatility')

# 4 Fit the GARCH Model

