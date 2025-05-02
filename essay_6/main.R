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
  Price = CAT$CAT.Close,
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
acf(na.omit(data$Return), lag.max = 40, main='ACF of Return Values',col='violetred1')
pacf(na.omit(data$Return), main='Partial Auto Correlation of Return Values',col='violetred1')
# Observe: ACF shows Return series is not dependent on previous days error
#          PACF shows Return series autocorrelates with lags 
#          Thus, Return series are influenced by previous days Return

# 3.3 Normality Test 
ggplot(aes(Return), data=data) + geom_histogram(bins = 100,col='black',fill='violetred1') + ggtitle('Return of MSFt')
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
a<- ggAcf(abs(na.omit(data$Return)), col='violetred1',main='Acf of Absolute Return of CAT')
p<- ggPacf(abs(na.omit(data$Return)),col='thistle1',main='PAcf of Absolute Return of CAT')
grid.arrange(a,p, ncol = 2, nrow = 1)
# Observe: Absolute Return series show slow decay of auto correlation 

c <- ggAcf(na.omit(data$Return)^2, lag.max = 40, col='violetred1', main='ACF of squared Return Values')
d<- ggPacf(na.omit(data$Return)^2,lag.max = 40, col='thistle1',main= 'PACF of squared Return Values')
grid.arrange(c,d, ncol = 2, nrow = 1)
# Observe: Squared Return series show slow decay of auto correlation 

# 3.6 Volatility Clustering 
chart.RollingPerformance(na.omit(data$Return),width = 22,FUN = 'sd.annualized',scale=252, main = 'Rolling 1 month Volatility')
# Observe: We can significantly improve our ARMA model using GARCH 

# 4 Fit the GARCH Model

#      Model 1: Fit ARMA(0,0)-gjrGARCH(1,1) model with Student t-distribution
CAT_garch_1 <- ugarchspec(mean.model =
                            list(armaOrder=c(0,0)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_1 <- ugarchfit(spec = CAT_garch_1, data= na.omit(data$Return))
aic_1 <- infocriteria(fit_garch_1)[1]
fit_garch_1
#plot(fit_garch_1,which='all')

#      Model 2: Fit ARMA(1,1)-gjrGARCH(1,1) model with Student t-distribution
CAT_garch_2 <- ugarchspec(mean.model = list(armaOrder=c(1,1)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_2 <- ugarchfit(spec = CAT_garch_2, data= na.omit(data$Return))
aic_2 <- infocriteria(fit_garch_2)[1]
aic_2
fit_garch_2
#plot(fit_garch_2,which='all')

#      Model 3: Fit ARMA(2,2)-gjrGARCH(1,1) model with Student t-distribution
CAT_garch_3 <- ugarchspec(mean.model = list(armaOrder=c(2,2)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_3 <- ugarchfit(spec = CAT_garch_3, data= na.omit(data$Return))
aic_3 <- infocriteria(fit_garch_3)[1]
fit_garch_3
#plot(fit_garch_3,which='all')

#      Model 4: Fit ARMA(0,0)-eGARCH(1,2) model with Student t-distribution
CAT_garch_4 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 2)),distribution = 'std')
fit_garch_4 <- ugarchfit(spec = CAT_garch_4, data= na.omit(data$Return))
aic_4 <- infocriteria(fit_garch_4)[1]
fit_garch_4
#plot(fit_garch_4,which='all')

#      Model 5: Fit ARMA(1,1)-eGARCH(2,1) model with Student t-distribution
CAT_garch_5 <- ugarchspec(mean.model = list(armaOrder=c(1,1)),variance.model = list(model = 'eGARCH', garchOrder = c(2,1)),distribution = 'std')
fit_garch_5 <- ugarchfit(spec = CAT_garch_5, data= na.omit(data$Return))
aic_5 <- infocriteria(fit_garch_5)[1]
fit_garch_5
#plot(fit_garch_5,which='all')

#      Model 6: Fit ARMA(3,1)-eGARCH(1,1) model with Student t-distribution
CAT_garch_6 <- ugarchspec(mean.model = list(armaOrder=c(3,1)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_6 <- ugarchfit(spec = CAT_garch_6, data= na.omit(data$Return))
aic_6 <- infocriteria(fit_garch_6)[1]
fit_garch_6
#plot(fit_garch_6,which='all')

#      Model 7: Fit ARMA(3,2)-eGARCH(1,1) model with Student t-distribution
CAT_garch_7 <- ugarchspec(mean.model = list(armaOrder=c(3,2)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_7 <- ugarchfit(spec = CAT_garch_7, data= na.omit(data$Return))
aic_7 <- infocriteria(fit_garch_7)[1]
fit_garch_7
#plot(fit_garch_7,which='all')

#      Model 8: Fit ARMA(1,3)-eGARCH(1,1) model with Student t-distribution
CAT_garch_8 <- ugarchspec(mean.model = list(armaOrder=c(1,3)),variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_8 <- ugarchfit(spec = CAT_garch_8, data= na.omit(data$Return))
aic_8 <- infocriteria(fit_garch_8)[1]
fit_garch_8
#plot(fit_garch_8,which='all')


# 5 Model Selection
Model = c('fit_garch_1','fit_garch_2','fit_garch_3','fit_garch_4','fit_garch_5','fit_
garch_6','fit_garch_7','fit_garch_8')
AIC = c(aic_1, aic_2, aic_3, aic_4, aic_5, aic_6, aic_7, aic_8)
(model <- data.frame(Model,AIC))
which.min(model[,'AIC'])
# Observe: The output was 5, so our selected model is fit_garch_5

# 6 Analyze Selected Model 
#      Statistics & Plot
fit_garch_5
plot(fit_garch_5,which='all')   

#      Persistence of Volatility
persistence(fit_garch_5) 

#      Convergence of the Model
print(convergence(fit_garch_5))

# 7 Forecasting (50 days ahead)
forecast_1 <-ugarchforecast(fit_garch_5,data=data,n.ahead=50)
forecast_1

#      Rolling Forecast 
fit_roll <- ugarchfit(CAT_garch_5, data= na.omit(data$Return),out.sample =500)
fore_roll <- ugarchforecast(fit_roll, n.ahead=50, n.roll=50)
fore_roll

par(mfrow=c(1,2))
plot(fore_roll,which=1)
plot(fore_roll,which=2)

#      Bootstrap Forecast
par(mfrow=c(1,2))
fore_boot <- ugarchboot(fit_garch_5,data = na.omit(data$Return), method = c("Partial", "Full")[1], n.ahead = 50, n.bootpred = 500)
plot(fore_boot,which=2)
plot(fore_boot,which=3)
head(sigma(forecast_1))


