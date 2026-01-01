# lecture 3

# 1. properties of AR(1)

# 2. properties of MA(1)

# 3. 
yt = c(0)
a0 = 2
a1 = 1.5
ys = arima.sim(n=1000, model = list(ar = c(.4, .3)))

for(j in 2:1000) {
  sighat = sqrt(a0 + a1 * yt[j-1]^2 )
  yt[j] = rnorm(1,0,sighat) * rnorm(1)
}
plot(log(yt), type = "l")


plot( log(aus_airpassengers), type = "l")
plot ( diff( log(aus_airpassengers$Passengers)), type = "l")


turkey = global_economy %>% filter(Country == "Turkey")
plot(diff(turkey$GDP), type = "l")
