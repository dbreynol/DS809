# Data processing
library(quantmod)
library(broom) # for the 'tidy' function
library(tidyverse)

# GDP Data from FRED
gdp = getSymbols('A939RX0Q048SBEA', src = 'FRED')
gd = tidy(A939RX0Q048SBEA) %>% 
  filter(year(index) >= 2010, year(index) <= 2023) %>% 
  rename(date = index, gdp = value)


home = getSymbols('CSUSHPINSA', src = 'FRED') # S&P/Case-Shiller U.S. National Home Price Index
home_pr = tidy(CSUSHPINSA) %>% 
  filter(year(index) >= 2010, year(index) <= 2023) %>% 
  rename(date = index, shiller = value)

comb = inner_join(gd, home_pr, by = 'date') %>% 
  select(-c('series.x', 'series.y'))

# write.csv(comb, "fred_dat.csv")
