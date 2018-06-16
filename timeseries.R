#Exponential smoothening 
# https://en.wikipedia.org/wiki/Exponential_smoothing#Time_Constant
# http://stat.ethz.ch/R-manual/R-devel/library/stats/html/HoltWinters.html


#Source For Time Series
# http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html#forecasts-using-exponential-smoothing

# Differencing
# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

# ARIMA Evaluatin
# http://theses.ulaval.ca/archimede/fichiers/21842/apa.html




#----------------------
#loading Data
#----------------------
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
plot(kingstimeseries)


births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot(birthstimeseries)


souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") 
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries
plot(souvenirtimeseries)


#seasonal???uctuations and random ???uctuations seem to increase with the level of the timeserie.
#So Log transformation helps to reduce skew 
plot(log(souvenirtimeseries))



skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866)) 
plot.ts(skirtsseries)


#-------------------------------------------------------
#Simple moving average 
#-------------------------------------------------------
kingstimeseries
plot(kingstimeseries)
library("TTR")
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)




#------------------------------------------------------
# Simple exponential smoothening 
#------------------------------------------------------
#using Excel Data
library(readxl)
sdata <- read_excel("C:/Users/v m kishore/OneDrive/Data sets/Time_series.xlsx", sheet = "single Expo")
s<- ts(sdata$Actual)
sh <- HoltWinters(sdata$Actual, beta=FALSE, gamma=FALSE)
sh
plot(sh)
fsh<-forecast(sh,h=5)
plot(fsh)
#------------------------------------------------------
#Reference:
# https://en.wikipedia.org/wiki/Exponential_smoothing
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
plot(rainseriesforecasts)
rainseriesforecasts$SSE

#Forecasting 
library("forecast")
rainseriesforecasts2 <- forecast(rainseriesforecasts, h=8)
rainseriesforecasts2

rainseriesforecasts2$fitted
plot(rainseriesforecasts2)

#Evaluation 
acf(rainseriesforecasts2$residuals[2:100], lag.max=20)
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot(rainseriesforecasts2$residuals)
hist(rainseriesforecasts2$residuals,breaks = 10)







#-------------------------------------------------------
#Holt's Exponential Smoothing(double Expo)
#-------------------------------------------------------
#using Excel Data
library(readxl)
sdata <- read_excel("C:/Users/v m kishore/OneDrive/Data sets/Time_series.xlsx", sheet = "single Expo")
s<- ts(sdata$Actual)
sh <- HoltWinters(sdata$Actual,gamma=FALSE)
sh
plot(sh)
fsh<-forecast(sh, h=5)
plot(fsh)
#--------------------------------------------------------
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts 
skirtsseriesforecasts$SSE 
plot(skirtsseriesforecasts)

HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)

#---------------------------------------------------------------
# Test for no Seasonality 
#---------------------------------------------------------------
acf(skirtsseriesforecasts2$residuals[3:length(skirtsseriesforecasts2$residuals)], lag.max=20) 
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(skirtsseriesforecasts2$residuals) # make a time plot 
hist(skirtsseriesforecasts2$residuals,breaks=10) # make a histogram





#-------------------------------------------------------
#Holt-Winters Exponential Smoothing
#-------------------------------------------------------
plot(souvenirtimeseries)
logsouvenirtimeseries <- souvenirtimeseries
plot(logsouvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries) 
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)

#Forecasting 
#-----------
library("forecast")
souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts,h=48)
plot(souvenirtimeseriesforecasts2)
plot.ts(souvenirtimeseriesforecasts2$residuals) # make a time plot 
hist(souvenirtimeseriesforecasts2$residuals) # make a histogram
acf(souvenirtimeseriesforecasts2$residuals[13:84])

#------
# Non additive seasons
#------
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries,seasonal ="multiplicative" ) 
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)

#Forecasting 
#-----------
souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts,h=48)
plot(souvenirtimeseriesforecasts2)
plot.ts(souvenirtimeseriesforecasts2$residuals) # make a time plot 
hist(souvenirtimeseriesforecasts2$residuals) # make a histogram
acf(souvenirtimeseriesforecasts2$residuals[13:84])



#-------------------------------------------------------
#Differencing  
#-------------------------------------------------------
library('tseries')
plot(kingstimeseries)
kingstimeseries1 <- diff(kingstimeseries, differences=1)
kingstimeseries2 <- diff(kingstimeseries, differences=2)
plot.ts(kingstimeseries1)
plot.ts(kingstimeseries2)
#Stationary Test
adf.test(kingstimeseries1, alternative = "stationary")
adf.test(kingstimeseries1, alternative = "stationary")




#-------------------------------------------------------
#Using ACF pots 
#-------------------------------------------------------

library(readxl)
seasons <- read_excel("C:/Users/v m kishore/OneDrive/Data sets/Time_series.xlsx", sheet = "Seasons ")
View(seasons)
s<-ts(seasons$Data)
plot(s)
acf(s)
pacf(s)

