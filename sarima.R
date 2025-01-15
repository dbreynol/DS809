library(fpp3)

leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")


employed = ts(leisure$Employed, start = c(2001,1), frequency = 12)

dts = diff(employed, lag = 12)
dts2 = diff(dts)
plot(employed)
plot(dts2)
library(forecast)
m0 = auto.arima(employed)
