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
