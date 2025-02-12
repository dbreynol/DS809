
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
alpha_1 = .4
n = 1000
r_tm1 = 0
returns = array( dim = n )

for(t in 1:n) {
  r_t = rnorm(1, mean = 0, sd = sqrt(alpha_0 + alpha_1 * r_tm1^2 ))
  returns[t] = r_t
  r_tm1 = r_t
}

acf(returns)
(acf(returns ^ 2))

mv1 = fGarch::garchFit(data = returns, formula = ~garch(1,1))
summary(mv1)
plot(returns, type = "l")




install.packages("devtools")
devtools::install_github("five-dots/rpolygon.io")
#POLYGON_KEY = "yDOlUmAgEZklJkhw_LBwFmL5_hYFnqoU"
Sys.getenv("POLYGON_KEY")
library(rpolygon.io)
last_aapl <- rpolygon("/v1/last/stocks/AAPL")
prev_aapl <- rpolygon("/v2/aggs/ticker/AAPL/prev", args = list(unadjusted = "true"))
