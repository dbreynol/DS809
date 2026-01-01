# sv1

library(stochvol)
library(tidyverse)

#btc = read.csv("https://tinyurl.com/3epw5n4z")
btc = read.csv("hashrate3_25.csv")
btc$timestamp = mdy(btc$timestamp)
btc$btc = btc$usd_hashprice/btc$btc_hashprice

# 3.125 BTC per block
bpd = (24 * 60)/10 # blocks per day
# find daily BTCs mined
mined = array(3.125, dim = nrow(btc))
mined[btc$timestamp < mdy("5-11-2020")] = 12.5
mined[btc$timestamp >= mdy("5-11-2020") & btc$timestamp < mdy("4-20-2024")] = 6.25
btc$mined = mined

# BTC / PH / S
btc$network = ( btc$mined * bpd ) /  ( ( btc$btc_hashprice)  )
spd = 60 * 60 * 24
btc$net2 = btc$network / spd * 1000
plot(btc$network/1000) # div by 1k to go from PH/s

with(btc, plot(x = timestamp, y = network, type = "l"))

ggplot(filter(btc, year(timestamp) == 2024), aes(x = btc, y = btc_hashprice)) + 
  geom_point(aes(color = factor ( year(timestamp)) ) )

plot(btc$network)
y = diff(log(btc$btc))
plot(x = btc$timestamp[-1], y, cex = .1, xlab = "", ylab = "d/d changes",
     )

hash_sv <- svsample(y) #, designmatrix = "ar1")
plot(hash_sv)

x = diff(log(btc$btc_hashprice))

(ccf(x, y))

x = ts(x)
y = ts(y)

plot(x = stats::lag(y), y = x)


mstr = read.csv("cleanedMSTR.csv")
mstr$Date = mdy(mstr$Date)

ggplot(mstr, aes(x = Date, y = Close)) + geom_line() + theme_minimal()

ggplot(mstr, aes(x = Date, y = CoinPrice)) + geom_line() + theme_minimal()

ggplot(mstr, aes(x = CoinPrice, y = Close)) + 
  geom_point(aes(color = as.factor ( year(Date))))  +
  theme_minimal() + 
  geom_smooth()



library(bsts)


mstr = read.csv("https://tinyurl.com/4cyrzvmu")
y = mstr$Close 
x = mstr$CoinPrice

# fit the model
ss = list()
ss = AddLocalLevel(ss, y)
ss = AddDynamicRegression(ss, y ~ x)
                          #model.options = DynamicRegressionArOptions(lags = 1, sigma.prior = SdPrior(100, 1000)))

dynmodel <- bsts(y,
                 state.specification = ss,
                 niter = 1000)

ss1 = AddLocalLevel(list(), y)
dynmodel1 = bsts(y ~ x, state.specification = ss1, niter = 1000)
PlotBstsComponents(dynmodel1)
#PlotDynamicRegression(dynmodel1)
#PlotBstsState(dynmodel)

par(mfrow = c(1,2))
PlotBstsResiduals(dynmodel)
PlotBstsResiduals(dynmodel1)

coefs =  ( drop ( dynmodel$dynamic.regression.coefficients ) )
q25 = apply(coefs, 2, function(x) quantile(x, probs = .25))
q75 = apply(coefs, 2, function(x) quantile(x, probs = .75))
meds = apply(coefs, 2, median)
plot(meds, type = "l")
lines(q25, col = "blue")
lines(q75, col = "red")

library(jsonlite)
a = read_json("hash-rate-daily.json", simplifyVector = T)
hash = a$`hash-rate`
plot(hash$y)

last = mdy("3-16-2025")
first = last - 1093
hash$timestamp = seq(from=first, to = last, by = "day")
with(hash, plot(timestamp, y, type = "l"))

dd = function(a) {return ( c(NA,diff(a)) / lag(a,1) ) }
# relative volatility versus btc hashprice
combined = left_join(hash, btc, by = "timestamp") %>% 
  mutate(net_lr = dd(y)) %>% 
  mutate(btch_lr = dd(btc_hashprice))

plot(x = combined$timestamp ,combined$btc_hashprice, type = "l")

combined$adj = ifelse(combined$timestamp < mdy("4-22-2024"), 
                      combined$btc_hashprice * 0.5,
                      combined$btc_hashprice)

with(combined, plot(x = timestamp, y = btc_hashprice, type = "l"))
lines(x = combined$timestamp, y = combined$adj, col = "red")

ggplot(combined) + ylim(c(-1,1)) + 
  geom_line(aes(x = timestamp, y = net_lr), color = "red") + 
  geom_line(aes(x = timestamp, y = btch_lr), color = "blue")

# 2 questions: 
# 1) is the vol on this lower than the btc hashrate?
length( seq(from = mdy("1-2-2009"), to = mdy("3-15-2025"), by = "day") )

# 2) can we convert this number back to our BTC hashrate number?
# 3) plot the 'residual'?









