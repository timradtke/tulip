
test_that("closer to current level if previously no trend and level", {

  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  beta <- runif(n = n_params, min = 0, max = 1)
  level_current <- rcauchy(n = n_params, location = 0, scale = 5)

  trend_current <- update_trend(
    beta = beta,
    one_minus_beta = 1 - beta,
    level_current = level_current,
    level_previous = 0,
    trend_previous = 0
  )

  expect_true(
    all(abs(level_current - trend_current) <= abs(level_current - 0))
  )
})

test_that("increases when current level larger than smoothed previous level", {

  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  beta <- runif(n = n_params, min = 0, max = 1)
  level_previous <- rcauchy(n = n_params, location = 0, scale = 5)

  # current level increases compared to previous
  level_current <- level_previous +
    abs(level_previous) * runif(n = n_params, min = 0, max = 2)
  trend_previous <- (level_current - level_previous) *
    runif(n = n_params, min = -2, max = 2)

  trend_current <- update_trend(
    beta = beta,
    one_minus_beta = 1 - beta,
    level_current = level_current,
    level_previous = level_previous,
    trend_previous = trend_previous
  )

  expect_true(all(
    trend_current > trend_previous * (1 - beta)
  ))
})

test_that("decreases when current level smaller than smoothed previous level", {

  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  beta <- runif(n = n_params, min = 0, max = 1)
  level_previous <- rcauchy(n = n_params, location = 0, scale = 5)

  # current level decreases compared to previous
  level_current <- level_previous +
    abs(level_previous) * runif(n = n_params, min = -2, max = 0)
  trend_previous <- (level_current - level_previous) *
    runif(n = n_params, min = -2, max = 2)

  trend_current <- update_trend(
    beta = beta,
    one_minus_beta = 1 - beta,
    level_current = level_current,
    level_previous = level_previous,
    trend_previous = trend_previous
  )

  expect_true(all(
    trend_current < trend_previous * (1 - beta)
  ))
})

test_that("returns zero when beta and one_minus_beta are 0", {

  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  level_previous <- rcauchy(n = n_params, location = 0, scale = 5)
  level_current <- level_previous +
    level_previous * runif(n = n_params, min = -2, max = 2)
  trend_previous <- (level_current - level_previous) *
    runif(n = n_params, min = -2, max = 2)

  trend_current <- update_trend(
    beta = 0,
    one_minus_beta = 0,
    level_current = level_current,
    level_previous = level_previous,
    trend_previous = trend_previous
  )

  expect_true(all(
    trend_current == 0
  ))
})
