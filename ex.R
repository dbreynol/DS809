
a = c(1,2,3,4,5)
# a - ahat | lag 1
y = c(2,3,4,5)
x = c(1,2,3,4)
set1 = residuals( lm( y ~ x))[2:4]

# residuals from lm with lag 2 as the response and lag 1 as explanatory
y = c(1,2,3)
x = c(2, 3, 4)
set2 = residuals ( lm(y ~ x) )

cor(set1, set2)

sum( (set1 - mean(set1)) * (set2 - mean(set2))) / sqrt( sum( (set1 - mean(set1))^2) * sum ( (set2 - mean(set2))^2 ) )  # by hand



(acf(a, type = "partial"))
