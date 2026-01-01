library(forecast)
library(tidyverse)
library(tseries)
library(fpp3)

# 0. Find some transformation to make our time series stationary
# 1. visualization: time series / acf / pacf .... candidate models
# 2. Fit candidate models .... choose LogLik
# 3. Check the residuals: want the residuals to resemble white noise
#    centered 0; constant variance; rho(h) = 0; h > 1
# 4. Forecast! 

# first difference
vec = c(1,3,7,7,10)
diff(vec)

# random walk
# x_t = x_{t-1} + w_t
# nabla(x_t) = x_{t - 1} + w_t - x_{t-1} = w_t


# ARIMA(0,1,0)
btc = read.csv("https://tinyurl.com/3epw5n4z") %>% 
  mutate(timestamp = ymd(timestamp))


plot(btc$btc, type = "l")
plot(diff(log(btc$btc)), type = "l")
plot(diff(btc$btc), type = "l")

tseries::adf.test(diff(log(btc$btc)))
# H_0: not stationary
# H_A: stationary

y = diff(log(btc$btc))
acf(y)

m0 = Arima( log(btc$btc) , order = c(0, 1, 1)) # Loglik = 4049.18
forecast::checkresiduals(m0)

m1 = auto.arima(log(btc$btc)) # loglik = 4051.79/ 
forecast::checkresiduals(m1)
plot ( forecast::forecast(m1 , 100) ) 


# SARIMA
plot(fma::elec)
frequency(elec)

# 0. make this thing stationary
# transformation: first difference ( lag 12 difference ( log elec) )

y1 = diff( log(elec) )
y2 = diff( y1 , 12 )
plot(y2)
tseries::adf.test(y2)

# SARIMA(p, d, q)(P, D, Q)_12

# SARIMA(0,1,1)(0, 1, 2)_{12}
acf(y2, lag.max = 36)
ms0 = Arima(log(elec), order = c(0, 1, 1), seasonal = c(0,1,2)) # loglik 1131.03
checkresiduals(ms0)

ms1 = auto.arima(log(elec)) # loglik 1134
checkresiduals(ms1)

plot ( forecast(ms1, h = 24) )













