library(tseries)
require(urca)
library(tidyverse)
library(fpp2)
library(vars)
library(forecast)
require(lmtest)
library(readxl)
suppressWarnings(expr)



brent_prices <- read_xlsx("brent_prices.xlsx")
Petr4_prices <- read_xlsx("Petr4_prices.xlsx")


### Brent Oil Prices Graph ###

ggplot(brent_prices, aes(x=Date)) + 
  geom_line(aes(y = Close), color = "steelblue") + 
  labs(title = 'Brent Prices since 2007')



### Petrobrás stock prices graph ###

ggplot(Petr4_prices, aes(x=Date)) + 
  geom_line(aes(y = `Close`), color = "red") + 
  labs(title = 'Petrobras Prices since 2007')


### Managing the data ###

final_data <- merge(Petr4_prices, brent_prices, by = 'Date')
names(final_data) <- c('Date','Petrobras', 'Brent Oil Prices')
normalized_final_data <- data_frame(final_data$Date,final_data[,2] / final_data[1,2],final_data[,3]/final_data[1,3])
names(normalized_final_data) <- c('Date','Petrobras', 'Brent Oil Prices')


### Brent x Petrobrás Graph in normalized scale ###

ggplot(normalized_final_data, aes(x = Date)) +
  geom_line(aes(y = Petrobras, color = "Petrobras")) +
  geom_line(aes(y = `Brent Oil Prices`, color = "Brent Oil Prices")) +
  labs(x = "Year",
       y = "(%)",
       color = "Legend")


### Transforming the data to the logarithm scale and plotting Petr4 x Brent Graph ###

log_data <- log(final_data[,2:3])
log_final_data <- data_frame(final_data$Date,log(final_data[,2:3]))
log_data <- data_frame(final_data$Date,log(final_data[,2:3]))
names(log_final_data) <- c('Date','Petrobras', 'Brent Oil Prices')
names(log_data) <- c('Date','Petrobras', 'Brent Oil Prices')

ggplot(log_final_data, aes(x = Date)) +
  geom_line(aes(y = Petrobras, color = "Petrobras")) +
  geom_line(aes(y = `Brent Oil Prices`, color = "Brent Oil Prices")) +
  labs(x = "Year",
       y = "(%)",
       color = "Legend", 
       title = 'PETR4 x Brent Logarithm')



### Augmented dickey fuller test for Brent Prices ###

adf_test_brent <- ur.df(log_data$`Brent Oil Prices`,selectlags = "BIC")
summary(adf_test_brent)


### Augmented dickey fuller test for Brent Prices with drift

adf_test_brent <- ur.df(log_data$`Brent Oil Prices`,type="drift",selectlags = "BIC")
summary(adf_test_brent)


### Augmented dickey fuller test for Brent Prices with trend
adf_test_brent <- ur.df(log_data$`Brent Oil Prices`,type="trend",selectlags = "BIC")
summary(adf_test_brent)


### Augmented dickey fuller test for Petrobrás stock prices
adf_test_petr <- ur.df(log_data$Petrobras,type="none", selectlags = "BIC")
summary(adf_test_petr)



### Augmented dickey fuller test for Petrobrás stock prices with drift
adf_test_petr <- ur.df(log_data$Petrobras,type="drift", selectlags = "BIC")
summary(adf_test_petr)



### Augmented dickey fuller test for Petrobrás stock prices with trend
adf_test_petr <- ur.df(log_data$Petrobras,type="trend", selectlags = "BIC")
summary(adf_test_petr)



### Checking correlation
corr_data <- cor(final_data$Petrobras, final_data$`Brent Oil Prices`)
corr_log_data <- cor(log_data$Petrobras, log_data$`Brent Oil Prices`)



### Log Returns for the data 

log_ret_petr <- diff(log_data$Petrobras)
log_ret_brent <- diff(log_data$`Brent Oil Prices`)


### ACF, PACF and series of returns for Petrobras
ggAcf(log_ret_petr) + labs(title = "AutoCorrelation for Petrobras Log Diff")
ggPacf(log_ret_petr) + labs(title = "Partial AutoCorrelation for Petrobras Log Diff")
ts.plot(log_ret_petr)



### ACF, PACF and series of returns for Petrobras
log_ret_brent <- diff(log_data$`Brent Oil Prices`)
ggAcf(log_ret_brent) + labs(title = "AutoCorrelation for Brent Log Diff")
ggPacf(log_ret_brent) + labs(title = "Partial AutoCorrelation for Brent Log Diff")
ts.plot(log_ret_brent)


### After the transformation of data, we need to test the new data (log return scaled) for stationarity.


###Augmented dickey fuller test for Log Returns of Petrobrás stock prices with drift
adf_test_petr <- ur.df(log_ret_petr,type="drift", selectlags = "BIC")
summary(adf_test_petr)

### Augmented dickey fuller test for Log Returns of Petrobrás stock prices with trend
adf_test_petr <- ur.df(log_ret_petr,type="trend", selectlags = "BIC")
summary(adf_test_petr)


### Augmented dickey fuller test for Log Returns of Petrobrás stock prices
adf_test_petr <- ur.df(log_ret_petr,type="none", selectlags = "BIC")
summary(adf_test_petr)

### Augmented dickey fuller test for Log Returns of Brent prices with drift
adf_test_petr <- ur.df(log_ret_brent,type="drift", selectlags = "BIC")
summary(adf_test_petr)

### Augmented dickey fuller test for Log Returns of Brent prices with trend
adf_test_petr <- ur.df(log_ret_brent,type="trend", selectlags = "BIC")
summary(adf_test_petr)


### Augmented dickey fuller test for Log Returns of Brent prices 
adf_test_petr <- ur.df(log_ret_brent,type="none", selectlags = "BIC")
summary(adf_test_petr)

### Specifying ARIMA model for log returns of Petrobras 
petr_model <- arima(log_ret_petr, order=c(1,0,1),  include.mean = FALSE, method = "ML")
summary(petr_model)
coeftest(petr_model)
par(mfrow = c(2,1))
ggAcf(residuals(petr_model))
ts.plot(residuals(petr_model))
hist(residuals((petr_model)))


### Specifying ARIMA model for log returns of Brent
brent_model <- arima(log_ret_brent, order=c(2,0,0),  include.mean = FALSE, method = "ML")
summary(brent_model)
coeftest(brent_model)
par(mfrow = c(2,1))
ggAcf(residuals(brent_model))
ts.plot(residuals(brent_model))
hist(residuals((brent_model)))


### Real time series x Fitted Values for the model specified for Log Returns of Petrobras
ts.plot(log_ret_petr, fitted(petr_model), col=c("red", "black"), ylab = "Real x Model Brent")
ts.plot(residuals(petr_model), col="blue", ylab="Residuals")


### Real time series x Fitted Values for the model specified for Log Returns of Brent 
par(mfrow = c(2,1))
ts.plot(log_ret_brent, fitted(brent_model), col=c("red", "black"), ylab = "Real x Model Brent")
ts.plot(residuals(brent_model), col="blue", ylab="Residuals")

### Specifying and model to predict Petrobras prices with Brent Prices

par(mfrow=c(2,1))
model_arimax <- Arima(log_ret_petr, order = c(0,0,1), xreg =log_ret_brent, include.constant = FALSE)
ts.plot(log_ret_petr, fitted(model_arimax), col=c("red", "green4"), ylab = "Real x Modelo Petrobrás")
ts.plot(residuals(model_arimax), col="black", ylab="resíduo")
checkresiduals(model_arimax)

### For the forecast produce, we gonna get the J.P Morgan, Reuters and Barclays forecasts for Brent Oil prices for 2022.


forecast_jpm <- c(log(125) - 4.586599)   ### jp morgan forecast
forecast_reuters <- c(log(120) -  4.586599)  ### Rosneft forecast
forecast_barclays <- c(log(85) -  4.586599)




jpm_forecast <- forecast(model_arimax, h = 1, xreg = forecast_jpm, level = 0.95)
reuters_forecast <- forecast(model_arimax, h = 1, xreg = forecast_reuters, level = 0.95)
barclays_forecast <- forecast(model_arimax, h = 1, xreg = forecast_barclays, level = 0.95)

par(mfrow=c(3,1))
plot(jpm_forecast, col = c("red"), xlab = "J.P. Morgan")
plot(reuters_forecast, col = c("green"), xlab = 'Reuters')
plot(barclays_forecast, xlab = "Barclays")


petr_stock_jpm <- exp(3.404857 + 0.1058667)
petr_stock_reuters <- exp(3.404857 + 0.08393084)
petr_stock_barclays <- exp(3.404857 - 0.1013705)
cat("The stock price to Petrobrás prediction according to JPM in Brent prices is \n", petr_stock_jpm)
cat("\n")
cat("The stock price to Petrobrás prediction according to Reuters in Brent prices is \n", petr_stock_reuters)
cat("\n")
cat("The stock price to Petrobrás prediction according to Barclays in Brent prices is \n", petr_stock_barclays)



### Since Brazil is going throught an election year, we decide to add a auto-regressive component, since the PETR4 stock price can pass to depend to previous values.

elections <- Arima(log_ret_petr, order = c(0,0,2), xreg = log_ret_brent, include.constant = TRUE)

jpm_forecast_elections <- forecast(elections, h = 1, xreg = forecast_jpm, level = 0.95)
reuters_forecast_elections <- forecast(elections, h = 1, xreg = forecast_reuters, level = 0.95)
barclays_forecast_elections <- forecast(elections, h = 1, xreg = forecast_barclays, level = 0.95)

par(mfrow=c(3,1))
plot(jpm_forecast_elections, col = c("red"), xlab = "J.P. Morgan")
plot(reuters_forecast_elections, col = c("green"), xlab = 'Reuters')
plot(barclays_forecast_elections, xlab = "Barclays")

petr_stock_jpm <- exp(3.404857 + 0.1057695)
petr_stock_reuters <- exp(3.404857 + 0.08387313)
petr_stock_barclays <- exp(3.404857 - 0.1010948)
cat("The stock price to Petrobrás prediction according to JPM in Brent prices is \n", petr_stock_jpm)
cat("\n")
cat("The stock price to Petrobrás prediction according to Reuters in Brent prices is \n", petr_stock_reuters)
cat("\n")
cat("The stock price to Petrobrás prediction according to Barclays in Brent prices is \n", petr_stock_barclays)