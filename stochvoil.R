

library(stochvol)
library(tidyverse)

btc = read.csv("https://tinyurl.com/3epw5n4z")
btc$timestamp = ymd(btc$timestamp)

plot(btc$btc, type = "l")
y = diff(log(btc$btc_hashprice))
plot(y, type = "l")


m0 = svsample(y)
plot(m0)
pred = ( predict(m0, steps = 2) )

str(pred)

