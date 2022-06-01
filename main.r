library(tseries)
require(urca)
library(tidyverse)
library(fpp2)
library(vars)
library(forecast)
require(lmtest)
### Brent ###
ggplot(brent_prices, aes(x=Date)) + 
  geom_line(aes(y = Close), color = "steelblue") + 
  labs(title = 'Brent Prices since 2007')

### Petrobras ###
ggplot(Petr4_prices, aes(x=Date)) + 
  geom_line(aes(y = `Close`), color = "red") + 
  labs(title = 'Petrobras Prices since 2007')

### Normalized data for better graph visualization ###
final_data <- merge(Petr4_prices, brent_prices, by = 'Date')
names(final_data) <- c('Date','Petrobras', 'Brent Oil Prices')
normalized_final_data <- data_frame(final_data$Date,final_data[,2] / final_data[1,2],final_data[,3]/final_data[1,3])
names(normalized_final_data) <- c('Date','Petrobras', 'Brent Oil Prices')

### Brent x Petrobras Graph ###
ggplot(normalized_final_data, aes(x = Date)) +
  geom_line(aes(y = Petrobras, color = "Petrobras")) +
  geom_line(aes(y = `Brent Oil Prices`, color = "Brent Oil Prices")) +
  labs(x = "Year",
       y = "(%)",
       color = "Legend")

### Logarithm Data ###
log_data <- data_frame(final_data$Date,log(final_data[,2:3]))
names(log_data) <- c('Date','Petrobras', 'Brent Oil Prices')

ggplot(log_data, aes(x = Date)) +
  geom_line(aes(y = Petrobras, color = "Petrobras")) +
  geom_line(aes(y = `Brent Oil Prices`, color = "Brent Oil Prices")) +
  labs(x = "Year",
       y = "(%)",
       color = "Legend", 
       title = 'PETR4 x Brent Logarithm')

### ADF Test for Brent ###

adf_test_brent <- ur.df(log_data$`Brent Oil Prices`,type="trend",selectlags = "BIC")
summary(adf_test_brent)

adf_test_brent <- ur.df(log_data$`Brent Oil Prices`,type="drift",selectlags = "BIC")
summary(adf_test_brent)

adf_test_brent <- ur.df(log_data$`Brent Oil Prices`,type="none",selectlags = "BIC")
summary(adf_test_brent)

### ADF Test for Petrobras ###
adf_test_petr <- ur.df(log_data$Petrobras,type="trend",selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_data$Petrobras,type="drift", selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_data$Petrobras,type="none", selectlags = "BIC")
summary(adf_test_petr)

corr_data <- cor(final_data$Petrobras, final_data$`Brent Oil Prices`)
corr_log_data <- cor(log_data$Petrobras, log_data$`Brent Oil Prices`)

### Log Returns ###
log_ret_petr <- diff(log_data$Petrobras)
ggAcf(log_ret_petr) + labs(title = "AutoCorrelation for Petrobras Log Diff")
ggPacf(log_ret_petr) + labs(title = "Partial AutoCorrelation for Petrobras Log Diff")
ts.plot(log_ret_petr) %>% 
  labs(title = "Log of returns for Petrobras")

log_ret_brent <- diff(log_data$`Brent Oil Prices`)
ggAcf(log_ret_brent) + labs(title = "AutoCorrelation for Brent Log Diff")
ggPacf(log_ret_brent) + labs(title = "Partial AutoCorrelation for Brent Log Diff")

(ts.plot(log_ret_brent) %>% 
  labs(title = "Log of returns for Brent Oil Prices"))


### ADF Tests for Log Returns of both Series ###
adf_test_petr <- ur.df(log_ret_brent,type="trend",selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_ret_brent,type="drift", selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_ret_brent,type="none", selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_ret_petr,type="trend",selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_ret_petr,type="drift", selectlags = "BIC")
summary(adf_test_petr)

adf_test_petr <- ur.df(log_ret_petr,type="none", selectlags = "BIC")
summary(adf_test_petr)








#### Fitting the Best ARIMA model for both series ####

brent_model <- Arima(log_data$`Brent Oil Prices`, order=c(2,1,0), include.constant = TRUE)
summary(brent_model)
coeftest(brent_model)
checkresiduals(fitted(brent_model))
par(mfrow = c(2,1))
ts.plot(as.ts(log_data$`Brent Oil Prices`), fitted(brent_model), col=c("red", "black"), ylab = "Real x Modelo Brent")
ts.plot(residuals(brent_model), col="blue", ylab="resíduo")



petr_model <- Arima(log_data$Petrobras, order=c(0,1,1), include.constant = TRUE)
summary(petr_model)
coeftest(petr_model)
checkresiduals(fitted(petr_model))
par(mfrow = c(2,1))
ts.plot(as.ts(log_data$Petrobras), fitted(petr_model), col=c("red", "black"), ylab = "Real x Modelo Brent")
ts.plot(residuals(petr_model), col="blue", ylab="resíduo")

### Petrobrás x Brent Oil Prices ### 

par(mfrow=c(2,1))
model_arimax <- Arima(log_data$Petrobras, order = c(0,1,1), xreg = log_data$`Brent Oil Prices`)
ts.plot(log_data$Petrobras, fitted(model), col=c("red", "green4"), ylab = "Real x Modelo Petrobrás")
ts.plot(residuals(model), col="black", ylab="resíduo")
checkresiduals(model)

### Forecasting ###
forecast_jpm <- c(log(125))   ### jp morgan forecast
forecast_reuters <- c(log(120))  ### Rosneft forecast
forecast_barclays <- c(log(85))  ### barclays forecast

jpm_forecast <- forecast(model_arimax, h = 1, xreg = forecast_jpm, level = 0.95)
reuters_forecast <- forecast(model_arimax, h = 1, xreg = forecast_reuters, level = 0.95)
barclays_forecast <- forecast(model_arimax, h = 1, xreg = forecast_barclays, level = 0.95)

par(mfrow=c(3,1))
plot(jpm_forecast, col = c("red"), xlab = "J.P. Morgan")
plot(reuters_forecast, col = c("green"), xlab = 'Reuters')
plot(barclays_forecast, xlab = "Barclays")

### Ano de Eleições ### 
elections <- Arima(log_data$Petrobras, order = c(0,1,2), xreg = log_data$`Brent Oil Prices`)

jpm_forecast_elections <- forecast(elections, h = 1, xreg = forecast_jpm, level = 0.95)
reuters_forecast_elections <- forecast(elections, h = 1, xreg = forecast_reuters, level = 0.95)
barclays_forecast_elections <- forecast(elections, h = 1, xreg = forecast_barclays, level = 0.95)

par(mfrow=c(3,1))
plot(jpm_forecast_elections, col = c("red"), xlab = "J.P. Morgan")
plot(reuters_forecast_elections, col = c("green"), xlab = 'Reuters')
plot(barclays_forecast_elections, xlab = "Barclays")