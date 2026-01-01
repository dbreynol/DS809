library(tseries)
library(fredr)
library(forecast)




newdat = fredr(
  series_id = "ICNSA",
  observation_start = as.Date("2000-10-01"),
  observation_end = as.Date("2025-02-26")
)

new2 = filter(newdat, )
plot(newdat$value)

y = newdat$value
ss = AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
model1 <- bsts(y,
               state.specification = ss,
               niter = 1000)
plot(model1, "components")

pred1 <- predict(model1, horizon = 12)
plot(pred1, plot.original = 156)

#mnile = bsts(y, state.specification = ss, niter = 1000)
#plot(mnile)
#plot(mnile, "components")





newdat$index = 1:nrow(newdat)

ggplot(newdat, aes(date, (value) )) + geom_line() + geom_smooth(method = "lm")


m0 = auto.arima(newdat$value)


plot(forecast(m0, h = 24))

lm(newdat$value ~ 1:length(newdat$value))

summary(lm(newdat$value ~ 1:length(newdat$value)))

