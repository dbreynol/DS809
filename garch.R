
library(fGarch)

md = garchFit(data = Nile)
# plot 3
# 13 (are residuals normal)

predict(md)

md1 = garchFit(formula =  ~ arma(1,0) + garch(1,1), data = Nile)
summary(md1)
