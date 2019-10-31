# Axis bank stock beta.

axis.bank.data <- read.csv('D:/other/NSE/stocks/banks/axisBank/15-10-2017-TO-14-10-2019AXISBANKALLN.csv')
close.price <- axis.bank.data[, 'Close.Price']
close.price
plot(close.price)
acf(close.price, lag.max = 250)
pacf(close.price)

# Differencing the series.
close.price.diff <- diff(close.price)
acf(close.price.diff) 
pacf(close.price.diff)
plot(close.price.diff) # Seems white noise.

# Moving average order 1 model.
ma1 <- arima(close.price, order = c(0, 0, 1))
res.ma1 <- ma1$residuals
plot(res.ma1) # Bad fit.
acf(res.ma1) 
pacf(res.ma1)

# Auto-regressive model of order 1.
ar1 <- arima(close.price, order = c(1, 0, 0))
res.ar1 <- ar1$residuals
plot(res.ar1) # Much better.
acf(res.ar1)
pacf(res.ar1)

# Integrated model of order 1.
int1 <- arima(close.price, order = c(0,1,0))
res.int1 <- int1$residuals
plot(res.int1)
acf(res.int1)
pacf(res.int1)

# Time series cross-validation
library(forecast)
e <- tsCV(close.price, forecastfunction=naive, h=8)
mse <- colMeans(e^2, na.rm = T)
rmse <- sqrt(mse)
rmse

e <- tsCV(close.price, forecastfunction=naive, h=1)
mse <- sqrt(mean(e^2, na.rm=TRUE))
mse
e

# Using a forecast function for tsCV()

ff <- function() {
  fit <- Arima(close.price, order = c(0, 0, 1))
  return(forecast(fit))
}
forecast.func <-function(y, h){forecast(Arima(y[1:482], order = c(0,0,1)),h=h)}
forecast.error <- tsCV(close.price, forecast.func, h=1) #Arguments to the forecast function are the arguments in the tsCV() function.








