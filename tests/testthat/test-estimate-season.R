
make_scenario <- function() {
  n <- sample(x = c(1, 2, 5, 6, 7, 11, 12, 13, 23, 24, 25, 27, 28, 36, 144,
                    573, 3772, 6284), size = 1)
  x <- rnorm(n = n)

  if (runif(n = 1) > 0.9) {
    x[sample(x = seq_along(x),
             size = sample(seq_along(x), size = 1))] <- NA_real_
  }

  m <- sample(x = c(1, 2, 7, 12, 24, 365), size = 1)
  na_fill <- sample(x = c(0, 1, NA_real_), size = 1)

  return(list(
    x = x,
    m = m,
    na_fill = na_fill
  ))
}

test_that("is idempotent", {
  expect_season_estimate_of_season_estimate_is_season_estimate <- function(x, m, na_fill) {
    scenario <- make_scenario()

    season_estimate <- estimate_season(
      x = scenario$x, m = scenario$m, na_fill = scenario$na_fill
    )

    season_estimate_of_season_estimate <- estimate_season(
      x = season_estimate, m = scenario$m, na_fill = scenario$na_fill
    )
    expect_identical(season_estimate, season_estimate_of_season_estimate)
  }

  for (i in 1:100) {
    expect_season_estimate_of_season_estimate_is_season_estimate()
  }
})

test_that("returns vector of length of x for any input vector x and value m", {
  expect_length_equal_length_of_x <- function(x, m, na_fill = 0) {
    expect_true(length(estimate_season(x = x, m = m, na_fill = na_fill)) ==
                  length(x))
  }

  combinations <- expand.grid(
    m = c(1, 2, 6, 7, 12, 365),
    n = c(1, 2, 3, 5, 6, 7, 10, 11, 12, 13, 23, 24, 48, 49, 364, 365, 366, 1000)
  )

  for (case_idx in 1:nrow(combinations)) {
    expect_length_equal_length_of_x(
      x = rnorm(combinations$n[case_idx]),
      m = combinations$m[case_idx]
    )
  }
})

test_that("throws error when input series is of length 0", {
  expect_error(estimate_season(x = numeric(), m = 1, na_fill = 0))
  expect_error(estimate_season(x = numeric(), m = 12, na_fill = 0))
  expect_error(estimate_season(x = numeric(), m = 0, na_fill = 0))
  expect_error(estimate_season(x = numeric(), m = , na_fill = NA_real_))
})

test_that("throws error when season length is less than 1", {
  expect_error(estimate_season(x = rnorm(23), m = 0, na_fill = 0))
  expect_error(estimate_season(x = rnorm(23), m = -1, na_fill = 0))
  expect_error(estimate_season(x = rnorm(23), m = -12, na_fill = 0))
})

test_that("returns all na_fill when input series is entirely NA", {
  expect_result_is_all_na_fill <- function(x, m, na_fill) {
    expect_identical(
      estimate_season(x = x, m = m, na_fill = na_fill),
      rep(na_fill, length(x))
    )
  }

  expect_result_is_all_na_fill(x = rep(NA_real_, 354), m = 1, na_fill = 1)
  expect_result_is_all_na_fill(x = rep(NA_real_, 354), m = 1, na_fill = 0)
  expect_result_is_all_na_fill(x = rep(NA_real_, 354), m = 1, na_fill = NA_real_)

  expect_result_is_all_na_fill(x = rep(NA_real_, 4), m = 15, na_fill = 1)
  expect_result_is_all_na_fill(x = rep(NA_real_, 4), m = 14, na_fill = 0)
  expect_result_is_all_na_fill(x = rep(NA_real_, 4), m = 18, na_fill = NA_real_)

  expect_result_is_all_na_fill(x = rep(NA_real_, 354), m = 13, na_fill = 1)
  expect_result_is_all_na_fill(x = rep(NA_real_, 354), m = 12, na_fill = 0)
  expect_result_is_all_na_fill(x = rep(NA_real_, 354), m = 11, na_fill = NA_real_)

  expect_result_is_all_na_fill(x = rep(NA_real_, 5), m = 1, na_fill = 1)
  expect_result_is_all_na_fill(x = rep(NA_real_, 6), m = 1, na_fill = 0)
  expect_result_is_all_na_fill(x = rep(NA_real_, 2), m = 1, na_fill = NA_real_)
})
