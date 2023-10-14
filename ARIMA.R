# Load required libraries
library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)
library(rugarch)
library(prophet)
library(tsfknn)

# Load financial data for the stock 'META' from Yahoo Finance
getSymbols("META", src = "yahoo", from = "2021-11-05", to = "2022-11-05")

# Display the first few rows of the 'META' dataset
head(META)

# Plot the time series of the closing prices of the 'META' stock
ggtsdisplay(META$META.Close)

# 1. Verify Stationarity

# --- Analytical Approach --- #
# Apply the Augmented Dickey-Fuller test to check for stationarity
adf.test(META$META.Close)

# Case of differencing:
# Apply 1st differencing to make the data stationary
differenced <- diff(META$META.Close)
# Perform the Augmented Dickey-Fuller test after differencing
adf.test(differenced)

# 2. Identify the Model

# --- Autocorrelation Function (ACF) & Partial ACF --- #

# Create ACF and PACF plots for the original time series data
acf(META$META.Close)
pacf(META$META.Close)

# Estimate the model for the original data
meta_model_original <- auto.arima(META$META.Close, ic = "aic", trace = TRUE)

# 3. Examine Model Diagnostics

# Analyze the Residuals:

# Extract residuals from the model
res <- meta_model_original$residuals

# Plot the ACF of the residuals
acf(res)

# Create a QQ plot of the residuals
qqnorm(res)
qqline(res)

# Perform the Shapiro-Wilk normality test for residuals
shapiro.test(res)

# 4. Generate Forecasts

# Forecast the original time series using the estimated model
Forecasted <- forecast(meta_model_original, level = c(95), h = 10)

# Plot the forecasted values and prediction intervals
plot(Forecasted)

# Display the forecasted values
Forecasted

# Model Validation
# Apply the Ljung-Box test for the residuals at different lags
Box.test(Forecasted$residuals, lag = 5, type = "Ljung-Box")
Box.test(Forecasted$residuals, lag = 15, type = "Ljung-Box")
Box.test(Forecasted$residuals, lag = 25, type = "Ljung-Box")
