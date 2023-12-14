library(stats)


library(statespacer)

set.seed(1)
y = arima.sim(n = 100, list(ar = c(.9)), sd = 1)
m0 = arima(y, order = c(1,0,0))


fit <- statespacer(y = matrix(y),
                   H_format = matrix(0),
                   local_level_ind = TRUE,
                   arima_list = list(c(1, 0, 0)),
                   format_level = matrix(0),
                   #initial = c(0.5*log(var(y)), 0, 0, 0),
                   verbose = TRUE,
                   standard_errors = TRUE)

m0
fit$system_matrices$AR
fit$standard_errors$AR
fit$diagnostics$loglik
fit$diagnostics$AIC
