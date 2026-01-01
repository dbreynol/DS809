# simulate white noise
ss = 10
wn = rnorm(ss)
acf(wn)

# simulate from AR1 with drift
a = .1
phi = 0.5
xt = array(dim = ss) # initialize a vector
xt[1] = 0

for(t in 2:ss) {
  xt[t] = a + phi * xt[t-1] + rnorm(1)
}

(acf(xt))

set.seed(1)
xt = rnorm(100)
# compute the lag 1 autocovariance
(acf(xt, type = "covariance"))

1/100 * sum( (xt[2:100] - mean(xt)) * (xt[1:99] - mean(xt)) )
acf(xt)

