--read clean dataset into R and look at graphs/summary
av_data = read.csv(file="C:/Users/jmmis/Documents/Data Science/DS700--Intro to Data Science/Final Project/Dataset_csv_abbeville.csv")
attach(av_data)
summary(av_data)
plot(Incoming.Examinations~Year)
ts_av_data <- ts(av_data[,1], start=c(2006, 1), frequency=12)
plot(ts_av_data)

--impute values through Amelia
install.packages("Amelia")
library(Amelia)
AmeliaView()

--Loaded the forecast package, loaded my forecast data into R and created a time series called “myforecast”
install.packages("forecast")
myforecast <- read.table("C:/Users/jmmis/Documents/Data Science/DS700--Intro to Data Science/Final Project/Dataset_csv_abbeville--Forecast Data.csv", header=TRUE)
myTS<-ts(myforecast)
myTS

--create lot of time series, look at ACF, PCF and number of diffs
plot(myTS, ylab="Number of Exams", xlab="Months")
acf(myTS)
pacf(myTS)
require(forecast)
ndiffs(x=myTS)
plot(diff(myTS, 1))

--generate first model, leveraging p,d,q values from R
myBestForecast <- auto.arima(x=myTS)
myBestForecast

--look at forecast model against residuals to gauge impact
acf(myBestForecast$residuals)
pacf(myBestForecast$residuals)
coef(myBestForecast)

--generate forecast of next 12 months using first model and plot
Next12months <- forecast (myBestForecast, h=12)
summary(Next12months)
plot(Next12months)

--generate new model using my own p,d,q values
myBestForecast2 <- arima(myTS, order=c(2,1,1))
myBestForecast2
acf(myBestForecast2$residuals)
pacf(myBestForecast2$residuals)

--generate second forecast using ARIMA and plot
Next12months2 <- forecast(myBestForecast2, h=12)
summary(Next12months2)
plot(Next12months2)

--generate third model/forecast using ARIMA and plot
myBestForecast3 <- arima(myTS, order=c(1,2,1))
myBestForecast3
Next12months3 <- forecast(myBestForecast3, h=12)
summary(Next12months3)
plot(Next12months3)

--Holt Winters forecast
--read file into R
holtforecast <- read.csv("C:/Users/jmmis/Documents/Data Science/DS700--Intro to Data Science/Final Project/Dataset_csv_abbeville--Forecast Data.csv", header=T)

--create time series
plot.ts(holtforecast)

--create model using R to generate values for alpha and beta
holtforecast.mean <- HoltWinters(holtzforecast, gamma=FALSE)
holtforecast.mean

--create forecast using R for next 12 months
holtforecast.predict <- forecast(holtforecast.mean, n.ahead=12, prediction.interval=TRUE)
holtforecast.predict

--plot original time series with Holt Winters curve laid on top
holtforecast.predict_plot <- predict(holtforecast.mean, n.ahead=12, prediction.interval=TRUE)
plot.ts(holtforecast, xlim=c(0, 120), ylim=c(0, 8000))
lines(holtforecast.mean$fitted[,1], col="green")
lines(holtforecast.predict_plot[,1], col="blue")
lines(holtforecast.predict_plot[,2], col="red")
lines(holtforecast.predict_plot[,3], col="red")

