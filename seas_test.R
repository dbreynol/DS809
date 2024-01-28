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

x = x + rep(c(-1,0,1,2), 5)
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

# model converstions using seasonality
library(tidyverse)

conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv")

# base model - Nile

# auto arima?

byday = conversions %>% filter(country_code == "us") %>% 
  mutate(datestamp = ymd(datestamp)) %>% group_by(datestamp) %>% summarise(conv = sum(conversions)) %>% 
  mutate(wd = wday(datestamp, label = T)) %>% drop_na()

# C matrix
mat = model.matrix(conv ~ factor(wd, ordered = F), data = byday)
mat[,1] = 1 #ifelse( rowSums(mat[, 2:7]) == 0 , 1, 0)
c.in = t(mat)
C = matrix ( c("Sun", "M", "T", "W", "Th", "F", "S"), nrow = 1)


mod.list <- list(
  U = matrix(0), ##
  x0 = matrix("x0"),
  B = matrix(1), ##
  Q = matrix("q"),
  Z = matrix(1), ##
  A = matrix(0), ##
  R = matrix("r"),
  C = (C),
  c = ( c.in ),
  tinitx = 0
)

y_ts = byday$conv - mean(byday$conv)
fit2 <- MARSS(y_ts, model = mod.list)

fr = forecast(fit2, h = 2, newdata = c.in[,c(1,2)], interval = "prediction")



mod.list2 <- list(
  U = matrix(0), ##
  x0 = matrix("x0"),
  B = matrix(1), ##
  Q = matrix(0),
  Z = matrix(1), ##
  A = matrix(0), ##
  R = matrix("r"),
  tinitx = 0
)

#y = ts(byday$conv[1:50], frequency = 7)
#y2 = rnorm(100)

fit3 <- MARSS(y2, model = mod.list2)

############## test using statespacer

library(tidyverse)
library(statespacer)
conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv")

byday = conversions %>% filter(country_code == "us") %>% 
  mutate(datestamp = ymd(datestamp)) %>% group_by(datestamp) %>% summarise(conv = sum(conversions)) %>% 
  mutate(wd = wday(datestamp, label = T)) %>% drop_na()


y <- as.matrix(byday$conv)
BSM_vec <- 7
#addvar_list <- list(as.matrix(Data[, c("PetrolPrice")]))

# Format of the variance - covariance matrix of the level component
# By setting the elements of this matrix to 0, 
# the component becomes deterministic.
format_level <- matrix(1)

# Format of the variance - covariance matrix of the seasonal component
# Note: This format must be a list of matrices, because multiple 
#       seasonalities can be specified!
format_BSM_list <- list(matrix(1))


# Fitting the model
fit1 <- statespacer(y = y,
                    local_level_ind = T,
                    BSM_vec = BSM_vec,
                    format_level = format_level, 
                    format_BSM_list = format_BSM_list,
                    method = "BFGS",
                    initial = .5 * log(var(y)),
                    verbose = TRUE)
fit1$system_matrices$Q$BSM7
plot(x = 1:301, byday$conv)

lines(1:301, fit1$filtered$level  , type = 'l', col = "blue")



plot(x = 1:301, fit1$filtered$BSM7, type = "l")
