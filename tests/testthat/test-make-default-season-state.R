
test_that("any series plus the additive default is the series", {
  for (i in 1:100) {
    n <- ceiling(rexp(n = 1, rate = 1/100))
    x <- rnorm(n = n)

    season <- make_default_season_state(n = length(x), method = "additive")

    expect_identical(x, x + season)
  }
})

test_that("any series times the multiplicative default is the series", {
  for (i in 1:100) {
    n <- ceiling(rexp(n = 1, rate = 1/100))
    x <- rnorm(n = n)

    season <- make_default_season_state(n = length(x),
                                        method = "multiplicative")

    expect_identical(x, x * season)
  }
})

test_that("throws error for `n` less than 1", {
  expect_error(make_default_season_state(n = 0, method = "additive"))
  expect_error(make_default_season_state(n = 0, method = "multiplicative"))
  expect_error(make_default_season_state(n = -3, method = "additive"))
  expect_error(make_default_season_state(n = -3, method = "multiplicative"))
})

test_that("throws error for `n` not integer", {
  expect_error(make_default_season_state(n = 1.5, method = "additive"))
  expect_error(make_default_season_state(n = 1.5, method = "multiplicative"))
  expect_error(make_default_season_state(n = pi, method = "additive"))
  expect_error(make_default_season_state(n = pi, method = "multiplicative"))
  expect_error(make_default_season_state(n = NA_integer_, method = "additive"))
  expect_error(make_default_season_state(n = NA_integer_, method = "multiplicative"))
  expect_error(make_default_season_state(n = NULL, method = "additive"))
  expect_error(make_default_season_state(n = NULL, method = "multiplicative"))
  expect_error(make_default_season_state(n = NaN, method = "additive"))
  expect_error(make_default_season_state(n = NaN, method = "multiplicative"))
  expect_error(make_default_season_state(n = Inf, method = "additive"))
  expect_error(make_default_season_state(n = Inf, method = "multiplicative"))
})

test_that("throws error for `method` not in choices", {
  expect_error(make_default_season_state(n = 12, method = "division"))
  expect_error(make_default_season_state(n = 7, method = NA_character_))
  expect_error(make_default_season_state(n = 63, method = ""))
  expect_error(make_default_season_state(n = 993, method = NULL))
})
