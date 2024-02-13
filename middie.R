library(tidyverse)
library(quantmod)

conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv")

conversions %>% filter(country_code == "be", marketing_channel == "Display Ads") %>% 
  mutate(datestamp = ymd(datestamp)) %>% 
  filter(month(datestamp) == 7) %>% 
  summarise(m = mean(conversions))
  

# what is the average number of conversions on Mondays in the US?
conversions %>% filter(country_code == "us") %>% 
  mutate(datestamp = ymd(datestamp)) %>% 
  filter(wday(datestamp, label = T) == "Tue") %>% 
  summarise(m = max(conversions))

infl = getSymbols("MICH", src = "FRED")
sent = getSymbols("UMCSENT", src = "FRED")
infl = data.frame(infl = MICH, date = ymd(index(MICH)))
sent = data.frame( sent = UMCSENT, date = ymd(index(UMCSENT)))

infl_sent = inner_join(sent, infl, by = "date" ) %>% 
  filter(year(date) >= 2010)

infl_sent %>% pivot_longer(-date) %>% 
  ggplot(aes(date, value)) + geom_line(aes(color = name)) 

with( infl_sent, plot(UMCSENT, MICH))

ggplot( filter(infl_sent, year(date)==2010), aes(UMCSENT, MICH )) + 
  geom_point(aes(color = factor(year(date))))


write.csv(infl_sent, "infl_csent.csv")

econ_dat = read.csv("https://raw.githubusercontent.com/dbreynol/admn510_data/main/infl_csent.csv")
j$date = ymd(j$date)

econ_dat %>% group_by(year(date)) %>% 
  summarise(c = cor(UMCSENT, MICH)) %>% 
  arrange(desc(c)) %>% View()



y = ts ( rep(c(-1,2,4,7,3), 20), frequency = 5)


set.seed(2)
y = arima.sim(n = 100, list(ar=c(.1,.2)))
auto.arima(y)

m0 = lm(UMCSENT ~ MICH, data = econ_dat)
(acf(residuals(m0)))


library(tidyverse)
library(gapminder)
library(fpp3)


sigsq = 4
T = 100
ran_walk = rnorm(1, 0, sigsq) # generate the first element of the random walk

for(i in 2:T) { ran_walk[i] = ran_walk[i-1] + rnorm(1, 0, sigsq) }

us_employment
us_employment$date = mdy ( str_c( month(us_employment$Month), "-1-", year(us_employment$Month)) )

retail = us_employment %>% 
  filter(Title == "Retail Trade", year(date) > 2002)



yt = c(-4, 0, 1, 3) + rnorm(4*20)
pacf(yt)


# test 
set.seed(1)
ytest = arima.sim(n = 1e3, list(ar = c(.3)))

m0 = Arima(ytest, order = c(1,0,0))

preds = fitted(m0)[1]
for(j in 2:1000) {
  preds[j] = c(1, ytest[j-1]) %*% c( coef(m0)[2], coef(m0)[1])
}

plot(1:1000, fitted(m0), type = "l")
lines(1:1000, preds, type = "l" , col = "red")

res1 = ytest - fitted(m0)
plot(1:1000, residuals(m0), type = "l", col = "blue")

arima(ytest, order = c(1, 0, 0))

y1 = window(ytest, start = 1, end = 999)
y2 = window(ytest, start = 2, end = 1000)

summary(lm(y1 ~ y2))
(acf(ytest))





y=arima.sim(list(ma=c(-.7,.9)), n=1e3)
pacf(y)
library(astsa)
library(forecast)
plot(gtemp_land)
m0 = Arima(gtemp_land, order = c(0,1,1), include.drift = T)
residuals(m0)
forecast(m0, h=1)

# diff = -0.7544795 * 0.3832516352
acf(diff(gtemp_ocean))
2.26 -0.2891555 + .0145
m1 = auto.arima(gtemp_ocean)
tail(residuals(m1))
0.78 + (0.09814299 * -0.425555886 + 0.04876906 * -0.309296738 + 0.004540101)

sqrt( 0.01516 * (1 + -0.4256 ^ 2 + -0.3093 ^ 2) ) * 1.96

tu = global_economy %>% filter(Country == "Turkey")
plot(tu$GDP, type = "l")
auto.arima(log(tu$GDP))

acf(diff(log(tu$GDP)))

gl = Arima(gtemp_land, order = c(1,1,0), include.drift = T)

(0.13 * -0.5058 + 0.0160 * (1--0.5058) ) + 2.26
forecast(gl, h=1)



y = arima.sim(1e4, model = list(ar = c(.4, 3.), ma = c(.6, .5, .4)))
par(mfrow = c(1,2))
acf(y, main = "ACF")
pacf(y, main = "PACF")


t = 50
lambda = 1:t * 2
y = rpois(t, lambda)

plot( y - mean(y), type = "l")

plot(diff(y), type = "l")

mean = 1:t * .2
y2 = rnorm(t, mean, sd=2)
plot(y2, type = "l", main = "Time Series Plot", xlab = "Time")


mo = rep(1:12, 12)
yr = rep(1949:1960, each = 12)
df = data.frame(mo, yr, ap = as.numeric(AirPassengers))
df %>% group_by(mo) %>% summarise(m = median(ap)) %>% facet_wrap(~factor(yr))




set.seed(2)
y = arima.sim(n = 10, model = list(c(ar = .9)))
Box.test(y)


diff(LakeHuron)
m0 = arima(LakeHuron, order = c(1,1,0))

library(tidyverse)
# Take 1: 2/12
conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv")

conversions %>% 
  mutate(datestamp = ymd(datestamp)) %>% 
  filter(country_code == "be", month(datestamp) == 7, marketing_channel == "Display Ads") %>%
  summarise(m2 = mean(conversions))
  group_by(datestamp) %>% summarise(c = sum(conversions)) %>% 
  summarise(m = mean(c))
  

conversions %>% filter(country_code == "us") %>% 
    mutate(datestamp = ymd(datestamp)) %>% 
  group_by(datestamp) %>%  summarise(tot = sum(conversions)) %>% 
    mutate(wd = wday(datestamp, label = T)) %>% 
    filter(wd == "Tue") %>% View()


econ_dat = read.csv("https://raw.githubusercontent.com/dbreynol/admn510_data/main/infl_csent.csv")

head(econ_dat)
m0 = lm(UMCSENT ~ MICH, econ_dat)
summary(m0)
econ_dat %>% mutate(date = ymd(date)) %>% mutate(yr = year(date)) %>% 
  group_by(yr) %>% summarise(c = cor(UMCSENT, MICH)) %>% View()


Arima(LakeHuron, order = c(1,1,0))

n = 50
n * (n+2) * sum(.13^2/(n-1)+.07^2/(n-2) + .04^2/(n-3))

pchisq(1.25, df = 3, lower.tail = F)

Arima(LakeHuron, order = c(1, 0, 1))

library(tidyverse)


conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv")
c2 = mutate(conversions, datestamp = ymd(datestamp))


c2 %>% filter(country_code == "be", 
              marketing_channel == "Display Ads",
              wday(datestamp, label == T) == "Tue") %>% 
  group_by(datestamp) %>% 
  summarise(s = sum(conversions)) %>% summarise(s = mean(s))

c2 %>% filter(country_code == "us", 
              wday(datestamp, label = T) == "Tue") %>% 
  group_by(datestamp) %>% 
  summarise(s = sum(conversions)) %>% summarise(s = mean(s))




b = c(0,25,60)
pr = c(.54, .34, .12)
eb = sum(b * pr)
sqrt ( sum ( (b - eb) ^ 2 * pr ) )


# L4
# corticosteroids in AUS
cortico = PBS %>% filter(ATC2 == "H02", Type == "Co-payments", Concession == "Concessional") %>% 
  mutate(cost2 = Cost/1e6)


plot(cortico$cost2, type = "l")

lcost = ts ( log(cortico$cost2) , frequency = 12)
d1_lcost = diff(lcost)
acf(d1_lcost)
sd1_lcost = ts( diff(d1_lcost, 12), frequency = 12)
acf(sd1_lcost)

m0 = auto.arima(lcost)
plot ( forecast(m0, h= 12) )




hist(Boston$lstat)
hist(Boston$medv)
m0 = lm(medv ~ lstat, Boston)
summary(m0)
hist(residuals(m0))


