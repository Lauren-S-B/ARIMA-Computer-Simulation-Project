---
title: "R Notebook"
output: html_notebook
---

##Report Analysis

Loading in libraries
```{r}
library(quantmod)
library(tseries)
library(forecast)
library(xts)
library("highcharter")
```

##Data Preparation and Exploratory Data Analysis
Obtaining the Apple Inc data from the getSymbols function.
```{r}
getSymbols('AAPL', from='2018-05-01', to='2023-05-18') #last 5 yrs
class(AAPL)
```

Viewing the entire Apple Inc data set
```{r}
#View(AAPL)
head(AAPL)
```

Modifying data set
```{r}
AAPL_C_P = AAPL[,4]
```

```{r}
head(AAPL_C_P)
```

Obtain a summary of Apple Inc data.
```{r}
summary(AAPL_C_P)
```

View number of dimensions
```{r}
dim(AAPL_C_P)
```

Missing values check
```{r}
colSums(is.na(AAPL_C_P))
```

Finding standard deviation of Apple Inc stock closing price
```{r}
sd(AAPL_C_P)
```

```{r}
range(AAPL_C_P)
```


##Data Visualisation

Visual plot of Apple Inc data set
```{r}
#basic coloured apple data plot
chartSeries(AAPL)
chartSeries(AAPL, subset='last 3 months')
```

Basic interactive plot of Apple Inc data set
```{r}
highchart() %>%
  hc_add_series(data = AAPL) %>%
  hc_add_theme(hc_theme_538())
```

Interactive plot of entire Apple Inc data set
```{r}
hc <- highchart(type = "stock") %>%
  hc_add_series(data = AAPL, color = "#5F83EE",
fillOpacity = 0.001) %>%
  hc_add_theme(hc_theme_538())
hc
```

Initial plot of Apple Inc closing price data
```{r}
plot(AAPL_C_P, main = "Raw Apple Closing Price", xlab="Time Frame", ylab = "Stock Price", col="blue") 
```

Checking for the presence of outliers
```{r}
par(oma = c(1,1,0,0) + 0.1, mar = c(1,1,1,1) + 0.1)
boxplot(AAPL_C_P,main = "Close", col = "lightblue")
```
Histogram of Apple Inc closing price data
```{r}
hist(AAPL_C_P, main = "Apple Stock Closing Price (2018-2023)", xlab = "Closing Price", xlim = c(min(AAPL_C_P) - 1 ,max(AAPL_C_P) + 1), col = "pink", border = "#263238", breaks = 50)
```


##Time series Analysis

Finding the log of Apple stock closing prices
```{r}
log.AAPL_C_P <- log(AAPL_C_P)
```

Plotting log values of closing Apple Inc stock price
```{r}
plot.ts(log.AAPL_C_P, main = "Apple Closing Price Log Plot",col=c(rep("blue")))
```

Changing trend to time series
```{r}
apple_ts <- ts(log.AAPL_C_P, start=c(1900), frequency=1)
autoplot(apple_ts, col="blue")
#looks non stationary as increasing fluctuation
```

Check if the data is stationary
```{r}
print(adf.test(log.AAPL_C_P, k=1))#k=1 have yearly data
#as p-value gets smaller, accuracy increases.
#p value greater 0.01 so series is non stationary
```

Perform 1st Difference on results and check if it is stationary
```{r}
apple_ts_d1 <- diff(apple_ts, difference = 1)
adf.test(apple_ts_d1, k = 1)
#p-value is 0.01 so is now stationary
#in ARIMA(p, d, q), the 'd' term will be "1"
```

Testing without the log
```{r}
apple_ts_NOLOG <- ts(AAPL_C_P, start=c(1900), frequency=1)
autoplot(apple_ts_NOLOG)
#looks non stationary as increasing fluctuation
apple_ts_NOLOG_d1 <- diff(apple_ts_NOLOG, difference=1)
adf.test(apple_ts_NOLOG, k=1)
autoplot(apple_ts_NOLOG_d1)
```

Testing with the log
```{r}
autoplot(apple_ts_d1)
#time series is now more or less stationary
```

Defining AR/lag/"p" term in the ARIMA(p,d,q) model
```{r}
Pacf(apple_ts_d1)
#p = 1
```

Defining the MA/"q" term in the ARIMA(p,d,q) model
```{r}
#Choosing q or MA term w ACF
acf(apple_ts_d1)
#choose closest to "0" to keep simple
#q=0
```
Auto ARIMA (1,1,0) model
```{r}
model<-auto.arima(y = log.AAPL_C_P, seasonal=FALSE)
model
```

##Diagnostic check of model
```{r}
et=residuals(model)
acf(et) # autocorrelation function
plot.ts(et)
gghistogram(et)
```

##Simulation of 10 random results
If anyone wishes to reproduce the results of the simulations, the option to reproduce the same results is included as set.seed and can be uncommented.
```{r}
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
```

```{r}
fit <- auto.arima(y[501:1500])
fit
```

```{r}
plot.ts(y[501:1500])
lines(fit$fitted, col="green")
```

##One simulation
```{r}
#Simulation fitted equation of size 1000
N=1500
eps=rnorm(N, 0, sqrt(0.0004346))
#theta = -1
y=rep(0,N)
for (t in 3:N){
  y[t]=0.0011+ y[t-1] + eps[t] -0.1178*eps[t-1]
}
```

```{r}
plot.ts(y[501:1500])
```

```{r}
fit <- auto.arima(y[501:1500])
fit
```

```{r}
plot.ts(y[501:1500])
lines(fit$fitted, col=3)
```
Additional forecast
```{r}
plot(forecast(fit, h=100))
```
