
library(tidymodels)
library(tidylog)

cereal = read.csv("https://raw.githubusercontent.com/DBomber60/WIHS/refs/heads/master/cereal.csv")

m0 = lm(rating ~ calories + protein + fat + sodium + fiber + carbo + sugars + potass + vitamins, data = cereal)

MASS::stepAIC(m0)


cor(cereal[,4:15])
