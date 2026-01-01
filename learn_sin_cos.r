# learn
library(fma)

head(elec)

m0 = lm(elec ~ time(elec) + cos(2 * pi * time(elec)))
summary(m0)

xr = data.frame( fourier(elec, K = 1) )

m1 = lm(elec ~ time(elec) + xr$S1.12 + xr$C1.12)
summary(m1)

plot(x = time(elec), y = elec, type = "l")
lines(predict(m0), col = "blue")

plot(predict(m0), col = "red", type = "l")
lines(predict(m1), col = "blue", type = "l")
