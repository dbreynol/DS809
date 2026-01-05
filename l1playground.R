# lecture 1 playground with PJM data
library(tidyverse)
library(magrittr)

load = read.csv('data/hrl_load_metered.csv')

load2 = load %>%
  mutate(datetime_beginning_ept = mdy_hms(datetime_beginning_ept)) %>% 
  filter(nerc_region == "RTO")


with(load2, plot(x = datetime_beginning_ept, y = mw, type = "l"))
