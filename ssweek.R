library(fredr)

homes = fredr(
  series_id = "CSUSHPINSA",
  observation_start = as.Date("2019-10-01"),
  observation_end = as.Date("2025-01-01")
)

ggplot(homes, aes(date, value)) + geom_line()


