test_that("adjusts towards y if level and trend are zero", {
  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  gamma <- runif(n = n_params, min = 0, max = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  season_previous <- y * runif(n = n_params, min = 1.01, max = 2)

  season_current <- update_season(
    gamma = gamma,
    one_minus_gamma = 1 - gamma,
    y = y,
    level_previous = 0,
    trend_previous = 0,
    season_previous = season_previous,
    method = "additive"
  )

  expect_true(all(
    abs(y - season_current) < abs(y - season_previous)
  ))
})

test_that("new season makes for better prediction than previous season", {
  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  gamma <- runif(n = n_params, min = 0, max = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  level_previous <- y * runif(n = n_params, min = -1, max = 2)
  season_previous <- y * runif(n = n_params, min = 1.01, max = 2)
  trend_previous <- y * runif(n = n_params, min = -1, max = 2) * 0.1

  season_current <- update_season(
    gamma = gamma,
    one_minus_gamma = 1 - gamma,
    y = y,
    level_previous = level_previous,
    trend_previous = trend_previous,
    season_previous = season_previous,
    method = "additive"
  )

  expect_true(all(
    abs(y - (level_previous + trend_previous + season_current)) <
      abs(y - (level_previous + trend_previous + season_previous))
  ))
})

test_that("seasonal random walk (alpha=1) means season adjusts to current residual", {
  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  level_previous <- y * runif(n = n_params, min = -1, max = 2)
  season_previous <- y * runif(n = n_params, min = 1.01, max = 2)
  trend_previous <- y * runif(n = n_params, min = -1, max = 2) * 0.1

  season_current <- update_season(
    gamma = 1,
    one_minus_gamma = 0,
    y = y,
    level_previous = level_previous,
    trend_previous = trend_previous,
    season_previous = season_previous,
    method = "additive"
  )

  expect_true(all(
    abs(season_current - (y - level_previous - trend_previous)) < 0.0001
  ))
})

test_that("fixed season (alpha=0) means season does not change", {
  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  level_previous <- y * runif(n = n_params, min = -1, max = 2)
  season_previous <- y * runif(n = n_params, min = 1.01, max = 2)
  trend_previous <- y * runif(n = n_params, min = -1, max = 2) * 0.1

  season_current <- update_season(
    gamma = 0,
    one_minus_gamma = 1,
    y = y,
    level_previous = level_previous,
    trend_previous = trend_previous,
    season_previous = season_previous,
    method = "additive"
  )

  expect_true(all(
    abs(season_current - season_previous) < 0.0001
  ))
})
