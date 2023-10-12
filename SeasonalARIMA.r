library(readxl)
library(forecast)
library(TSA)
library(GEVcdn)
library(tseries)
library(tsoutliers)

#lire le fichier Excel 
data <- read_excel("TmpChange.xlsx")
View(data) 

# Transformation des données du char a numeric
data$TmpChange <- sapply(data$TmpChange, as.numeric )

#check de missing na values
sum(is.na(data))²
#summary statistics
summary(data)

# creation d el object time series
series=ts(data$TmpChange,start=2010,frequency=12)

ggtsdisplay(series)

# close to 1 , perfectly linear 
BoxCox.lambda(series)

#acf de la series
acf=acf(series)
pacf=pacf(series)
decomposition= decompose(series)
ggtsdisplay(acf)
ggtsdisplay(pacf)
ggtsdisplay(decomposition)


#diffeerencing 12 lag
differenced<-diff(series,lag=12,differences=1)
ggtsdisplay(differenced)

# decompsition modele additive
trend=decomposition$trend
seasonal =decomposition$seasonal
random=decomposition$random

#test de stationnarité 
adfuller_stationarity=adf.test(series)


#check du 1er fit manuel
fit1<-Arima(series,order=c(0,2,0),seasonal=c(1,1,0),lambda = NULL,include.constant = TRUE)
#visualization des residus fit1
ggtsdisplay(fit1$residuals)
#test de significativité des coeff du fit 1
coeftest1=coeftest(fit1)
#test de ljunx box sur les residus 
res1=checkresiduals(fit1)
summary(fit1)


#le deuxieme fit manuel
fit2<-Arima(series,order=c(0,1,0),seasonal=c(0,1,1),lambda = NULL,include.constant = True)
coeftest2=coeftest(fit2)
res2=checkresiduals(fit2)
ggtsdisplay(fit2$residuals)
summary(fit2)



# Fit automatique auto arima :
model=auto.arima(series,D=1)
model_coeff=coeftest(model)
residuals=model$residuals
checkresiduals(model)



#forecasting du model 
forecasting=forecast(model,h=7)
plot(forecasting)



forecast_residuals= forecasting$residuals

#test de goodness of fit du modele 

JarqueBera.test(forecast_residuals)

