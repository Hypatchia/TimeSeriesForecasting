#libraries
library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)
library(rugarch)
library(prophet)
library(tsfknn)

#lodaing data
getSymbols("META",src="yahoo",from="2021-11-05",to = "2022-11-05")
head(META)
META

#plotting the time series
ggtsdisplay(META$META.Close)

# 1 ---- Tester La stationarité:

#---Analytical Approach---#
#adfuller augmented test:
adf.test(META$META.Close)

#Cas de differencing :
#1st differencing 
differenced<-diff(META$META.Close)
#Adf test after differencing 
adf.test(differenced)


# 2 --- Idetification of model :
#---Autocorrelation Function & Partial ACF---#

acf(META$META.Close)
pacf(META$META.Close)

#2 --- Estimation:

#for Original Data :
meta_model_original<-auto.arima(META$META.Close,ic="aic",trace=TRUE)

# 3 --- Diagnostics :

#Analyse des Residus:

res<-meta_model_original$residuals
acf(res)
qqnorm(res)
qqline(res)
shapiro.test(res)


# 4 --- Forecasting :
#Original series :
Forecasted <-forecast(meta_model_original,level=c(95),h=10)
plot(Forecasted)
Forecasted

#Model Validation
Box.test(Forecasted$residuals, lag=5, type= "Ljung-Box")
Box.test(Forecasted$residuals, lag=15, type= "Ljung-Box")
Box.test(Forecasted$residuals, lag=25, type= "Ljung-Box")

