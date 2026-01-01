mean(c(34,39,41,35,41))
median(c(34,39,41,35,41))

1/6 ^ 3
1/216

# question 9
# P(d or e) = d + e - dande
# q10, 11
.2 / .6

(.62 + .56 - .7)/.62
(28-32.5) / 2.5

(((100 * 1.065 * 1.093 * (1-.17))/100)^(1/3)-1)*100

num = c(16,56,99,14,20,62)
margin = c(-1,0,1,2,3,4)
sum ( (num / sum(num) )  * margin )

# define correlation

(64 * 25 + 74)/26
sd(c(78,69,96,96,66,82))
10/(3 * 4)

((100 * 1.055 * 1.062 * .93 * 1.19)/100)^(1/4)

.8 - .12

# P(E | F) = P(e and f) / p(f)

(.4 * .3) / (.5 * .6 + .4 * .3 + .1 * .2)

.6 * .3 + .5 * .3 + .4 * .4
#P(hi | accepted)
(30/400) / (70/400)



cereal = read.csv("https://raw.githubusercontent.com/DBomber60/WIHS/refs/heads/master/cereal.csv")


with(cereal, cor(rating, fiber))

m0 = (lm(rating ~ fiber, cereal))
predict(m0, newdata = data.frame(fiber = 14))
80 - 83.45829 


str(cereal)
cor(cereal[,4:16])

# calories/ sugars
m1 = (lm(rating ~ calories + sugars, cereal))
summary(lm(rating ~ sugars, cereal))


plot(x = cereal$sugars, y = residuals(m1))

