library(tseries)
library(tidyverse)

btc = read.csv("https://tinyurl.com/3epw5n4z")



head(btc)
tail(btc)
plot(x = ymd(btc$timestamp), y = btc$btc, type = "l")

y = btc$btc
y2 = diff(btc$btc)
plot(y2, type = "l")


tseries::kpss.test(y2)
tseries::adf.test(y2)


acf(y2)
pacf(y2)

auto.arima(y2)
m0 = Arima(y, order = c(0,1,1), include.drift = T)
m1 = auto.arima(y)



plot(expsmooth::visitors)
y2 = diff(expsmooth::visitors, 12)
plot(y2)
tseries::adf.test(y2)
tseries::kpss.test(y2)

auto.arima(expsmooth::visitors)



