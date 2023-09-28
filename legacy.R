# some legacy code

# predict next month's conversions (channel and country)
conv2 = conversions %>% 
  drop_na(conversions) %>% 
  mutate(date_ = ymd(datestamp)) %>% 
  filter(conversions < 15000)

# fit a model

m0 = lm(conversions ~ country_code, data = conv2)

conv2$predict = predict(m0)

gri = conv2 %>% group_by(country_code) %>% summarise(conversions = mean(predict))
ggplot(conv2, aes(country_code , conversions )) + geom_boxplot() + geom_point(data = gri, color = "red")


predict(m0, newdata = data.frame(country_code = "be", marketing_channel = "Display Ads" ))

conv2 %>% group_by(country_code, marketing_channel) %>% summarise(m = mean(conversions, na.rm = T))

# total visits by country
visits %>% group_by(country_code) %>% summarise(s = sum(user_visits), .groups = 'drop') %>% arrange(desc(s))

# channel breakdown by country 
visits %>% group_by(country_code, marketing_channel) %>% summarise(s = sum(user_visits), .groups = 'drop') 

visits %>% group_by(country_code) %>% mutate(s = sum(user_visits)) %>% 
  group_by(country_code, marketing_channel) %>% summarise(m = (sum(user_visits) / min(s)) * 100 ) %>%
  arrange(country_code,desc(m))

# channel visits by weekday
# dates
visits %>% mutate(date = ymd(datestamp)) %>% 
  group_by(date) %>% summarise(s = sum(user_visits)) %>% 
  ggplot(aes(x = date, y = s)) + geom_line()

visits %>% 
  mutate(date = ymd(datestamp)) %>% 
  mutate(day = wday(date, label = T)) %>% 
  group_by(marketing_channel, day) %>% 
  summarise(mean_v = mean(user_visits)) %>% 
  ggplot(aes(day, mean_v)) + geom_col() + facet_wrap(~marketing_channel)

# laura - peter / caitline - casey / cooper - callie / toby - jack / addison - will / brendan - ben


v2 = visits %>% 
  group_by(datestamp) %>% 
  summarise(s = sum(user_visits)) %>% 
  mutate(date = ymd(datestamp)) %>%
  mutate(day = factor(wday(date, label = T), ordered = F)) %>% 
  mutate(mon = factor(month(date)))

m0 = lm(s ~ day, data = v2)
m1 = lm(s ~ day * mon, data = v2)

visits$datestamp = ymd(visits$datestamp)
head(visits)
d2 = visits %>% group_by(datestamp, marketing_channel) %>% summarise(visits = sum(user_visits))

ggplot(filter(d2, marketing_channel == "Display Ads"), aes(x = datestamp, y = visits, color = marketing_channel)) + 
  geom_line()


d3 = filter(d2, marketing_channel == "KAYAK Deals Email")
acf(d3$visits)

getSymbols("AAPL")
head(AAPL)
AAPL2 = AAPL['2021/2023']
d = diff (AAPL2$AAPL.Adjusted) 
acf(d$AAPL.Adjusted[-1])