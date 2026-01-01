a = rnorm(100)

a1 = a[2:100]
a2 = a[1:99]


plot(a1, a2)

1/100 * sum( (a1 - mean(a)) * (a2 - mean(a)) )

(acf(a, type = "covariance"))





