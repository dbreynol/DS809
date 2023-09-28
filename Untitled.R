
sample( c(50, 1, -20), 1,  prob = c(1/6, 1/3, 1/2) )


mean ( sample( c(50, 1, -20), 5000,  prob = c(1/6, 1/3, 1/2) , replace = T) )

mu = 1 * 12/52 + 5 * 4/52 + 10 * 1/52

mean ( sample(c(1,5,10,0), 1000, prob = c(12/52, 4/52, 1/52, 35/52), replace = T) )



library(tidyverse)
visits = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/visits.csv")


visits_g = visits %>% 
  group_by(datestamp) %>% 
  summarise(visits = sum(user_visits)) %>% 
  mutate(date2 = ymd(datestamp))

plot(visits_g$date2, visits_g$visits, type = "l")

ggplot(visits_g, aes(date2, visits)) + geom_line() + ggtitle("visits")





