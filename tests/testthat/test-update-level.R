
test_that("closer to y after update if no trend and season", {
  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  alpha <- runif(n = n_params, min = 0, max = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  level_previous <- y * runif(n = n_params, min = -1, max = 2)

  level_current <- update_level(
    alpha = alpha,
    one_minus_alpha = 1 - alpha,
    y = y,
    level_previous = level_previous,
    trend_previous = 0,
    season_previous = 0,
    method = "additive"
  )

  expect_true(
    all(abs(y - level_current) <= abs(y - level_previous))
  )
})

test_that("new level makes for better prediction than previous level", {
  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  alpha <- runif(n = n_params, min = 0, max = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  level_previous <- y * runif(n = n_params, min = -1, max = 2)
  season_previous <- y * runif(n = n_params, min = 1.01, max = 2)
  trend_previous <- y * runif(n = n_params, min = -1, max = 2) * 0.1

  level_current <- update_level(
    alpha = alpha,
    one_minus_alpha = 1 - alpha,
    y = y,
    level_previous = level_previous,
    trend_previous = trend_previous,
    season_previous = season_previous,
    method = "additive"
  )

  expect_true(all(
    abs(y - (level_current + trend_previous + season_previous)) <
      abs(y - (level_previous + trend_previous + season_previous))
  ))
})

test_that("returns zero when both alpha and one_minus_alpha are zero", {

  n_params <- sample(x = c(1, 5, 10, 10000), size = 1)
  y <- rcauchy(n = n_params, location = 0, scale = 5)
  level_previous <- y * runif(n = n_params, min = -1, max = 2)
  season_previous <- y * runif(n = n_params, min = 1.01, max = 2)
  trend_previous <- y * runif(n = n_params, min = -1, max = 2) * 0.1

  level_current <- update_level(
    alpha = 0,
    one_minus_alpha = 0,
    y = y,
    level_previous = level_previous,
    trend_previous = trend_previous,
    season_previous = season_previous,
    method = "additive"
  )

  expect_true(all(
    level_current == 0
  ))
})
