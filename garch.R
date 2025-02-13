
library(fGarch)

md = garchFit(data = Nile)
# plot 3
# 13 (are residuals normal)

predict(md)

md1 = garchFit(formula =  ~ arma(1,0) + garch(1,1), data = Nile)
summary(md1)

# arch(1)
# y_t = sigma_t * w_t
# sigma_t^2 = alpha_0 + alpha_1 * y_{t-1}^2
set.seed(1)
alpha_0 = .1
alpha_1 = .5
n = 10000
r_tm1 = 0
returns = array( dim = n )

for(t in 1:n) {
  r_t = rnorm(1, mean = 0, sd = sqrt(alpha_0 + alpha_1 * r_tm1^2 ))
  returns[t] = r_t
  r_tm1 = r_t
}

plot(returns, type = "l")
acf(returns)
(acf(returns ^ 2))

m0 = fGarch::garchFit(data = returns, formula = ~garch(1,0))

plot(returns, type = "l")

btc = read.csv("https://tinyurl.com/3epw5n4z") %>% 
  mutate(timestamp = ymd(timestamp))


mbv = fGarch::garchFit(diff(log(btc$btc)), formula = ~ arma(0,2) + garch(1,1)) # 4076
predict(mbv, n.ahead = 10)

# Number of periods to forecast
n_forecast <- 10
sigma2_forecast <- numeric(n_forecast)

# First forecast
sigma2_forecast[1] <- omega + alpha1 * tail(data^2, 1) + beta1 * sigma2_last

# Iterative forecasts (assuming zero future shocks)
for (t in 2:n_forecast) {
  sigma2_forecast[t] <- omega + (alpha1 + beta1) * sigma2_forecast[t-1]
}

# Convert to standard deviation (volatility)
sigma_forecast <- sqrt(sigma2_forecast)




# Define ARCH(1) model specification
arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                        distribution.model = "norm")  # Assume normal errors