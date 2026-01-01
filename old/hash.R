# hashprac
library(tidyverse)

hash = read.csv("hashprice_20125.csv")
hash$timestamp = mdy(hash$timestamp)
hash = arrange(hash, timestamp)

hasht = ts(hash$btc_hashprice)


ggplot(hash, aes(timestamp, btc_hashprice)) + geom_line()

tseries::adf.test(hash$btc_hashprice)

plot(diff(hash$btc_hashprice))


tseries::adf.test(diff ( hash$usd_hashprice) )
acf(diff(hash$usd_hashprice))
acf(diff(hash$usd_hashprice), type = "partial")
m0 = forecast::auto.arima(hash$usd_hashprice)
#Arima(hash$btc_hashprice, order = c())
plot(forecast(m0, h = 24))


ARMAtoMA(ar = coefficients(m0)[1],
         ma = coefficients(m0)[2:4], lag.max = 2)








