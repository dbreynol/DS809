
library(tidyverse)
library(fpp3)

y2 = ts( newdat$value , start = c(2015, 10), frequency = 12)
mshiller = StructTS(y2, type = "BSM")
mshiller = tsSmooth(mshiller)
plot(forecast(mshiller, h = 12))
par(mfrow = c(1,3))
plot(mshiller[,1])
plot(mshiller[,2])
plot(mshiller[,3])
plot(mshiller$residuals)

y = log( window(fma::elec))
mbsm = StructTS(y, type = "BSM")

# forecast with this
par(mfrow = c(1,3))
plot(mbsm$fitted[,1])
plot(mbsm$fitted[,2])
plot(mbsm$fitted[,3])
plot(mbsm$residuals)
#smoothed = stats::tsSmooth(mbsm)
#plot(smoothed[,1])
#plot(smoothed[,2])
#plot(smoothed[,3])

mar = auto.arima(y)
checkresiduals(mar)
# plot(y)
# lines(mbsm$fitted[,1])
# lines(mbsm$fitted[,2])
# plot (rowSums ( mbsm$fitted) , type = "l", col = "blue")



ml= StructTS(Nile, type = "level")
ml$fitted
plot(Nile)
lines(ml$fitted, col = "blue")
plot(forecast(ml, h = 10))
# same with bsts

library(bsts)

y = Nile
ss = AddLocalLevel(list(), y)
mnile = bsts(y, state.specification = ss, niter = 1000)
plot(mnile)
plot(mnile, "components")
pred1 <- predict(mnile, horizon = 12)
plot(pred1, plot.original = 156)

ss2 = AddLocalLinearTrend(list(), y)
m2 = bsts(y, state.specification = ss2, niter = 1000)
plot(mnile, "components")

CompareBstsModels(list("Model 1" = mnile,
                       "Model 2" = m2
                       ),
                  colors = c("black", "red"))


data(iclaims)

# delete 3

x = 0:3
px =c(.7, .15, .09, .06)
sum ( x * px )
sum ( (x-.51)^2 * px ) 

dbinom(2, 7, .06)
dbinom(3,8,.08)
dbinom(2, 5, .04)
