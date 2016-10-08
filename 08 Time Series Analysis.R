#Time Series Data using class ts()
#frequency=7: a weekly series###I frequency=12: a monthly series####I frequency=4: a quarterly series

a <- ts(1:20, frequency = 12, start = c(2011, 3))
print(a)
str(a)
attributes(a)

#Decompose a time series into components
##I Trend component: long term trend
##I Seasonal component: seasonal variation
##I Cyclical component: repeated but non-periodic fluctuations
##I Irregular component: the residuals

plot(AirPassengers)
apts <- ts(AirPassengers, frequency = 12)
f <- decompose(apts)
plot(f$figure, type = "b") # seasonal figures
plot(f)

# time series forecasting 
# build an ARIMA model
fit <- arima(AirPassengers, order = c(1, 0, 0), list(order = c(2,1, 0), period = 12))
fore <- predict(fit, n.ahead = 24)
# error bounds at 95% confidence level
U <- fore$pred + 2 * fore$se
L <- fore$pred - 2 * fore$se

ts.plot(AirPassengers, fore$pred, U, L, col = c(1, 2, 4, 4), lty = c(1, 1, 2, 2))
legend("topleft", col = c(1, 2, 4), lty = c(1, 1, 2),c("Actual", "Forecast", "Error Bounds (95% Confidence)"))
