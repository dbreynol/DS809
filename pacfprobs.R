set.seed(1)
simmed = arima.sim(n = 10000, list(ar = c(0.7, 0.1)), sd = 1)

pacf(simmed)

set.seed(1)
x = rnorm(100, mean = 100, sd = 20)
z = x - mean(x)
y = rnorm(100, mean = 10000 - x, sd = 10)

summary(lm(y ~ x))

sum ( (x - mean(x)) * (y - mean(y)) ) 
sum( (x - mean(x)) ^ 2)
sum(y)
sum(x)

nfl = read.csv("https://cnow.apps.ng.cengage.com/ilrn/books/ca3mbs08h/passingnfl_r.csv")


summary( lm(Win ~ Yds_Att, nfl) ) 
lm(Win ~ Int_Att, nfl)
summary ( lm(Win ~ Yds_Att + Int_Att, nfl) ) 

car = read.csv("https://cnow.apps.ng.cengage.com/ilrn/books/ca3mbs08h/autoresale_r.csv")

coefficients ( lm(Price ~ Mileage + Age, car) )

tv = read.csv("https://cnow.apps.ng.cengage.com/ilrn/books/ca3mbs08h/showtime_r.csv")
m0 = lm(Revenue ~ TV + Newspaper, tv)
predict(m0, newdata = data.frame(TV = 3.2, Newspaper = 2.7), interval = "confidence")






library(bsts)

y = Nile
ss = list()
ss = AddLocalLevel(ss, y)
llmodel <- bsts(y,
                 state.specification = ss,
                 niter = 1000)

PlotBstsComponents(llmodel)

hist(llmodel$sigma.obs)
median(llmodel$sigma.obs^2) # 15242
hist(llmodel$sigma.level)
median(llmodel$sigma.level^2) # 1477.533

# local level version
mll = StructTS(y, type = "level")
str(mll)
plot(mll$fitted)

mll$coef
