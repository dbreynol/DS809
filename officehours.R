


ts_ex = c(2, 4, 6, 8, 10)

library(tidyverse)
fred = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/fred_dat.csv")[,-1]
fred$date = ymd(fred$date)

head(fred)
# lag 1 autocovariance of gdp
1/55 * sum( (fred$gdp[3:55] - mean(fred$gdp)) * (fred$gdp[1:53] - mean(fred$gdp) ) )
(acf( fred$gdp, type = "covariance"))


# 174985 ..... ccf lag 1 where shiller lags gdp
1/55 * sum( (fred$gdp[2:55] - mean(fred$gdp)) * (fred$shiller[1:54] - mean(fred$shiller)))

(ccf(fred$gdp, fred$shiller, type = "covariance"))



my_acf = function(vec, lags = c(1,2,3)) {
  # output will be lags 1,2,3 autocorrelation
}


a = read.csv("https://cnow.apps.ng.cengage.com/ilrn/books/ca3mbs08h/networks_r.csv")
table(a)/25 * 100
b = read.csv("https://cnow.apps.ng.cengage.com/ilrn/books/ca3mbs08h/snow_r.csv")
with(b, plot(Temp, Snowfall))



