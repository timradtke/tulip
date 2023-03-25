
set.seed(8274)

n_obs <- 84 + 12
x <- seq_len(n_obs)
dates <- seq(as.Date("2015-01-01"), as.Date("2022-12-01"), by = "month")

# basic seasonality and trend
y <- cospi(x / 6) * sqrt(x / 100) * 25 +
  sinpi(x / 3) * 25 +
  sqrt(x) * 10

# adding t-distributed noise and an offset for strictly positive data
y <- 5 + pmax(0, y + rt(n = n_obs, df = 3) * 10)

# adding anomalies to the end of the training data
y[84-1] <- y[84-1] * 3
y[84] <- y[84] * 1.5

flowers <- data.frame(
  date = dates[1:84],
  flowers = round(y[1:84])
)

flowers_holdout <- data.frame(
  date = dates[85:n_obs],
  flowers = round(y[85:n_obs])
)

save(flowers, file = "data/flowers.rda")
save(flowers_holdout, file = "data/flowers_holdout.rda")
