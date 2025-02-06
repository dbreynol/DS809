library(forecast)
library(tidyverse)

# ARIMA(0,1,0)
btc = read.csv("https://tinyurl.com/3epw5n4z") %>% 
  mutate(timestamp = ymd(timestamp)) %>% 
  mutate(lbtc = log(btc)) %>% 
  mutate(lbtc_1d = c(NA, diff(lbtc)))

plot(btc$btc, type = "l")
plot(diff(btc$btc), type = "l")
plot(diff(btc$lbtc), type = "l")

tseries::adf.test(btc$lbtc_1d[-1])

m0 = Arima(btc$lbtc, order = c(0,1,0))
m1 = Arima(btc$lbtc, order = c(0,1,1))

var(btc$lbtc_1d, na.rm = T)
forecast::checkresiduals(m0)
forecast::checkresiduals(m1)
acf(btc$lbtc_1d[-1], type = "covariance")[0:1]
plot(forecast(m0, h = 100))

m1 = auto.arima(btc$lbtc)

m1 = Arima(btc$lbtc_1d[-1], order = c(0,0,0))
plot(forecast(m1, h = 100))
checkresiduals(m1)


head(elec)
plot(elec)
plot((log(elec)))
z12 = diff(log(elec))
z122 = diff(z12, 12) 
acf(z122, lag.max = 36) # MA1/ 
acf(z122, type = "partial", lag.max = 48)
ms1 = auto.arima(z122)
ms0 = Arima(elec, order = c(0,1,1), seasonal = c(0,1,2))
checkresiduals(ms1)
tseries::adf.test(diff(log(elec),12))
auto.arima(elec)
mauto = auto.arima(log(elec))
plot(forecast(mauto, h = 12))

