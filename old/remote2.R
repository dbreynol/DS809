library(astsa)
# library(fredr)
# fredr_set_key("bd6a5a557f46d9b2ea931e46e50b2578")
# 
# newdat = fredr(
#   series_id = "USLAH",
#   observation_start = as.Date("2019-10-01"),
#   observation_end = as.Date("2025-01-01")
# )
#library(astsa) # sarima.sim
library(tidyverse)
library(forecast)


leisure = fpp3::us_employment %>% 
  filter(Title == "Leisure and Hospitality") %>% 
  mutate(log_emp = log(Employed))

plot(leisure$log_emp, type = "l")

y_d1 = diff(leisure$log_emp)
y_d2 = diff(y_d1, lag = 12)
plot(y_d2, type = "l")

adf.test(y_d2) # reject the null: non-stationary

acf(y_d2) # (0,1,2)(0,1,1)_{12}

# let's coerce the log of the employed to a time series object
y = ts(leisure$log_emp, start = c(1939, 1), frequency = 12)
m0 = Arima(y, order = c(0, 1, 2), # ll = 3627
      seasonal = c(0,1,1))

checkresiduals(m0)

m1 = auto.arima(y) # ll = 3649
summary(m1)
checkresiduals(m1)

plot ( forecast(m0, h = 36) ) 

sim1 = sarima.sim(sma = 0.7, n = 1000, S = 12)
acf(sim1, lag.max = 36)



sim1 = sarima.sim(sar = 0.7, n = 1000, S = 12)
acf(sim1, lag.max = 36, type = "partial")




# simulate a sequence of r_t from ARCH(1)
# E( r_t | r_{t-1}) = 0 
# Var(r_t | t_{t-1}) = \omega + alpha1 * r_{t-1}^2

omega = 0.2
alpha_1 = 0.4
n = 10000
returns = array(0,dim = n)

for( t in 2:n) {
  rt = rnorm(1, mean = 0, sd = sqrt(omega + alpha_1 * returns[t-1]^2))
  returns[t] = rt
}

acf(returns) # white noise
acf(returns^2) # structure in the variation!

library(fGarch)

m0 = fGarch::garchFit(data = returns, formula ~ garch(1,0))
summary(m0)
predict(m0, n.ahead = 30, plot = T)

library(tidyverse)
btc = read.csv("https://tinyurl.com/3epw5n4z")
btc$timestamp = ymd(btc$timestamp)
y = diff( log(btc$btc) )
m_arch = fGarch::garchFit(data = y, formula ~ garch(1,0)) # 4076
summary(m_arch)
predict(m_arch, n.ahead = 30, plot = T)

m_arch = fGarch::garchFit(data = y, formula ~ garch(1,1)) # 4151
summary(m_arch)
predict(m_arch, n.ahead = 60, plot = T)


m_2 = fGarch::garchFit(data = y, formula ~ arma(0,1) + garch(1,1)) # 4151
summary(m_2)
predict(m_2, n.ahead = 5, plot = T, crit_val = 1, mse = "uncond") # 4217

# 
sd = sarima.sim(ma = .7, sma = .9, n = 1000, S = 12)
acf(sd, type = "partial")

# q2: reviewing data
require(graphics)

trees <- window(treering, start = 0)
(fit <- StructTS(trees, type = "level"))
plot(trees)
lines(fitted(fit), col = "green")



library(tidyverse)
conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv")


wday(ymd("2025-2-18"), label = T)


library(forecast)
Arima(y, order = c(1,0,0))




recent = read.csv("https://raw.githubusercontent.com/dbreynol/admn510_data/refs/heads/main/btc_recent.csv")

library(fredr)

newdat = fredr(
  series_id = "CSUSHPINSA",
  observation_start = as.Date("2019-10-01"),
  observation_end = as.Date("2025-01-01")
)




