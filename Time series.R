install.packages("forecast")
library("forecast")
data(AirPassengers)
class(AirPassengers)
#Shows the start of the Air Passengers data
start(AirPassengers)
#Shows the end of the Air Passengers data
end(AirPassengers)
#The cycle of this time series is 12months in a year
frequency(AirPassengers)
sum(is.na(AirPassengers))
summary(AirPassengers)

plot(AirPassengers, col="aquamarine4")
abline(reg=lm(AirPassengers~time(AirPassengers)), col="aquamarine4")
#This will print the cycle across years.
cycle(AirPassengers)
#This will aggregate the cycles and display a year on year trend
plot(aggregate(AirPassengers, FUN=mean), col="aquamarine4")
#Box plot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))

adf.test((diff(AirPassengers)), alternative="stationary", k=0)
acf(log(AirPassengers))

acf(diff(log(AirPassengers)))
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))


tsdata <- ts(AirPassengers, frequency = 12)
ddata <- decompose(tsdata, "multiplicative")
#Decomposing means separating the original Time Series into its components(trend, seasonaltiy, irregularity)
plot(ddata, col="aquamarine4")

#Suggest the best values for p, d, q
mymodel <- auto.arima(AirPassengers)
mymodel
#Suggests the best value that minimizes AIC, if the trace="TRUE" then R shows how it computes parameters
auto.arima(AirPassengers, ic="aic", trace=TRUE)

install.packages("tseries")
library(tseries)
plot.ts(mymodel$residuals)
acf(ts(mymodel$residuals), main="ACF Residual")
pacf(ts(mymodel$residuals), main="PACF Residual", )

#It's possible to plot a forecast of the Time Series using the forecast function with a 95 % conf interval where h is the forecast horizon periods in months
myforecast <- forecast(mymodel, level=c(95), h=10*12)
plot(myforecast)

#Ljung-Box test is used to validate auto.arima model
#The null hypothesisi is: Model does not show the lack of fit
#The alternative hypothesis is that is shows the lack of fit
Box.test(mymodel$resid, lag=5, type="Ljung-Box")
Box.test(mymodel$resid, lag=10, type="Ljung-Box")
Box.test(mymodel$resid, lag=15, type="Ljung-Box")
