#Report Analysis

#Loading in libraries
library(quantmod)
library(tseries)
library(forecast)
library(xts)
library("highcharter")


#Data Preparation and Exploratory Data Analysis:

#Obtaining the Apple Inc data from the getSymbols function.
getSymbols('AAPL', from='2018-05-01', to='2023-05-18') #last 5 yrs
class(AAPL)

#Viewing the entire Apple Inc data set
View(AAPL)
head(AAPL)

#Modifying data set
AAPL_C_P = AAPL[,4]

head(AAPL_C_P)


#Obtain a summary of Apple Inc data.
summary(AAPL_C_P)

#View number of dimensions
dim(AAPL_C_P)


#Missing values check
colSums(is.na(AAPL_C_P))


#Finding standard deviation of Apple Inc stock closing price
sd(AAPL_C_P)


range(AAPL_C_P)



##Data Visualisation

#Visual plot of Apple Inc data set:

#basic coloured apple data plot
chartSeries(AAPL)
chartSeries(AAPL, subset='last 3 months')

#Basic interactive plot of Apple Inc data set
highchart() %>%
  hc_add_series(data = AAPL) %>%
  hc_add_theme(hc_theme_538())

#Interactive plot of entire Apple Inc data set
hc <- highchart(type = "stock") %>%
  hc_add_series(data = AAPL, color = "#5F83EE",
fillOpacity = 0.001) %>%
  hc_add_theme(hc_theme_538())
hc


#Initial plot of Apple Inc closing price data
plot(AAPL_C_P, main = "Raw Apple Closing Price", xlab="Time Frame", ylab = "Stock Price", col="blue") 


#Checking for the presence of outliers
par(oma = c(1,1,0,0) + 0.1, mar = c(1,1,1,1) + 0.1)
boxplot(AAPL_C_P,main = "Close", col = "lightblue")

#Histogram of Apple Inc closing price data
hist(AAPL_C_P, main = "Apple Stock Closing Price (2018-2023)", xlab = "Closing Price", xlim = c(min(AAPL_C_P) - 1 ,max(AAPL_C_P) + 1), col = "pink", border = "#263238", breaks = 50)



##Time series Analysis

#Finding the log of Apple stock closing prices
log.AAPL_C_P <- log(AAPL_C_P)


#Plotting log values of closing Apple Inc stock price
plot.ts(log.AAPL_C_P, main = "Apple Closing Price Log Plot",col=c(rep("blue")))


#Changing trend to time series
apple_ts <- ts(log.AAPL_C_P, start=c(1900), frequency=1)
autoplot(apple_ts, col="blue") #looks non stationary as increasing fluctuation


#Check if the data is stationary
print(adf.test(log.AAPL_C_P, k=1))#k=1 have yearly data
#as p-value gets smaller, accuracy increases.
#p value greater 0.01 so series is non stationary


#Perform 1st Difference on results and check if it is stationary
apple_ts_d1 <- diff(apple_ts, difference = 1)
adf.test(apple_ts_d1, k = 1)
#p-value is 0.01 so is now stationary
#in ARIMA(p, d, q), the 'd' term will be "1"


#Testing without the log
apple_ts_NOLOG <- ts(AAPL_C_P, start=c(1900), frequency=1)
autoplot(apple_ts_NOLOG)
#looks non stationary as increasing fluctuation
apple_ts_NOLOG_d1 <- diff(apple_ts_NOLOG, difference=1)
adf.test(apple_ts_NOLOG, k=1)
autoplot(apple_ts_NOLOG_d1)


#Testing with the log
autoplot(apple_ts_d1)
#time series is now more or less stationary


#Defining AR/lag/"p" term in the ARIMA(p,d,q) model
Pacf(apple_ts_d1)
#p = 1


#Defining the MA/"q" term in the ARIMA(p,d,q) model
#Choosing q or MA term w ACF
acf(apple_ts_d1)
#choose closest to "0" to keep simple
#q=0

#Auto ARIMA (1,1,0) model
model<-auto.arima(y = log.AAPL_C_P, seasonal=FALSE)
model


##Diagnostic check of model

et=residuals(model)
acf(et) # autocorrelation function
plot.ts(et)
gghistogram(et)


##Simulation of 10 random results
#If anyone wishes to reproduce the results of the simulations, the option to reproduce the same results is included as set.seed and can be uncommented.
#However, as we are trying to get random simulation results for each run it will be left commented for now.
#set.seed(1234)
par(mfrow=c(2,2))
i=0
p = 0
y = y[501:1500]
x<-while(i < 10){
  N=1500
  eps=rnorm(N, 0, sqrt(0.0004346))
  y=rep(0,N)
  for (t in 3:N){
    y[t]=0.0011+ y[t-1] + eps[t] - 0.1178*eps[t-1]
  }
  p<-plot.ts(y)
  i<-i + 1
}



fit <- auto.arima(y[501:1500])
fit



plot.ts(y[501:1500])
lines(fit$fitted, col="green")


##One simulation
#Simulation fitted equation of size 1000
N=1500
eps=rnorm(N, 0, sqrt(0.0004346))
#theta = -1
y=rep(0,N)
for (t in 3:N){
  y[t]=0.0011+ y[t-1] + eps[t] -0.1178*eps[t-1]
}


plot.ts(y[501:1500])


fit <- auto.arima(y[501:1500])
fit


plot.ts(y[501:1500])
lines(fit$fitted, col=3)


#Additional forecast
plot(forecast(fit, h=100))

