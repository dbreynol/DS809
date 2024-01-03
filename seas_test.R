# seasonal test

library(MARSS)

##  Let's fit a model where the hidden state is a random walk

# x is the "hidden" trend we want to find
# x(t)=x(t-1)+u+w(t), w(t)~N(0,q)
# x(0)=x0
years = 30
x0 <- 1
u <- 0
q <- .2
r <- .2
n <- 4 * years 


# number of 'seasons' (e.g., 12 months per year)
period <- 4
# first 'season' (e.g., Jan = 1, July = 7)
per.1st <- 1
# create factors for seasons
c.in <- diag(period)
for (i in 2:years) {
  c.in <- cbind(c.in, diag(period))
}

C = matrix(c("q1","q2","q3","q4"), nrow = 1)

t <- 1:n
x <- x0 + u + rnorm(1, 0, sqrt(q)) 
for (i in 2:n) x[i] <- x[i - 1] + u + rnorm(1, 0, sqrt(q))

x = x + rep(c(-1,0,1,2), 5)/2
plot(x, type = "l")

# y is our observation of x with error
y <- x + rnorm(n, 0, sqrt(r))

## NOW WE ESTIMATE Q and SET R TO ZERO
mod.list <- list(
  U = matrix(0),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"),
  C = C,
  c = c.in,
  tinitx = 0
)

fit2 <- MARSS(y, model = mod.list)


coef(fit2)$C




TT <- 2
covariate <- matrix(0, 12, TT)
monrow <- match(chinook.month$Month, month.abb)[1:TT]
covariate[cbind(monrow,1:TT)] <- 1
covariate[,1:12]











fulldat <- lakeWAplanktonTrans

years <- fulldat[, "Year"] >= 1965 & fulldat[, "Year"] < 1975
phytos <- c("Diatoms", "Greens", "Bluegreens", "Unicells", "Other.algae")
dat <- t(fulldat[years, phytos])
