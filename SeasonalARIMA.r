# Load required libraries
library(readxl)
library(forecast)
library(TSA)
library(GEVcdn)
library(tseries)
library(tsoutliers)

# Read data from an Excel file
data <- read_excel("TmpChange.xlsx")
View(data)

# Transform the data from character to numeric
data$TmpChange <- sapply(data$TmpChange, as.numeric)

# Check for missing values
sum(is.na(data))²

# Display summary statistics of the data
summary(data)

# Create a time series object
series = ts(data$TmpChange, start = 2010, frequency = 12)

# Display the time series
ggtsdisplay(series)

# Perform Box-Cox transformation to make the data linear
BoxCox.lambda(series)

# Calculate and display the autocorrelation and partial autocorrelation functions
acf = acf(series)
pacf = pacf(series)
decomposition = decompose(series)
ggtsdisplay(acf)
ggtsdisplay(pacf)
ggtsdisplay(decomposition)

# Differencing with a lag of 12
differenced <- diff(series, lag = 12, differences = 1)
ggtsdisplay(differenced)

# Decompose the time series into trend, seasonal, and random components
trend = decomposition$trend
seasonal = decomposition$seasonal
random = decomposition$random

# Test for stationarity using ADF test
adfuller_stationarity = adf.test(series)

# Perform the first manual fit
fit1 <- Arima(series,
order = c(0, 2, 0),
seasonal = c(1, 1, 0),
lambda = NULL,
include.constant = TRUE)

# Visualize the residuals of fit1
ggtsdisplay(fit1$residuals)

# Test the significance of coefficients in fit1
coeftest1 = coeftest(fit1)

# Test for Ljung-Box on the residuals of fit1
res1 = checkresiduals(fit1)

# Display a summary of fit1
summary(fit1)

# Perform the second manual fit
fit2 <- Arima(series,
order = c(0, 1, 0),
seasonal = c(0, 1, 1),
lambda = NULL,
include.constant = TRUE)

# Test the significance of coefficients in fit2
coeftest2 = coeftest(fit2)

# Test for Ljung-Box on the residuals of fit2
res2 = checkresiduals(fit2)

# Visualize the residuals of fit2
ggtsdisplay(fit2$residuals)

# Display a summary of fit2
summary(fit2)

# Automatic model selection using auto.arima
model = auto.arima(series, D = 1)
model_coeff = coeftest(model)
residuals = model$residuals
checkresiduals(model)

# Forecast using the selected model
forecasting = forecast(model, h = 7)
plot(forecasting)

# Calculate and test the Jarque-Bera goodness of fit of the model's residuals
forecast_residuals = forecasting$residuals
JarqueBera.test(forecast_residuals)
