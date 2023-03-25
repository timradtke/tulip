
set.seed(46295)

has_large_seasonal_component__gen_example_result <- function() {
  n_obs <- sample(x = 10000, size = 1)

  residuals <- runif(n = n_obs, min = -1, max = 1) * rexp(n = 1, rate = 1 / 100)
  residuals_after_seasonality <- runif(n = n_obs, min = -1, max = 1) * rexp(n = 1, rate = 1 / 100)

  any_nas <- runif(n = 1) > 0.75
  if (any_nas) {
    residuals[sample(x = n_obs, size = ceiling(n_obs * runif(1)))] <- NA_real_
    residuals_after_seasonality[sample(x = n_obs, size = ceiling(n_obs * runif(1)))] <- NA_real_
  }

  threshold <- runif(n = 1)

  seasonal_component_is_large <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals_after_seasonality,
    threshold = threshold
  )

  return(list(
    residuals = residuals,
    residuals_after_seasonality = residuals_after_seasonality,
    threshold = threshold,
    result = seasonal_component_is_large
  ))
}

ls_example_results <- replicate(
  n = 100,
  expr = has_large_seasonal_component__gen_example_result(),
  simplify = FALSE
)

test_that("returns logical scalar output for any kind of numeric inputs", {
  lapply(
    X = ls_example_results,
    FUN = function(x) {
      expect_true(is.logical(x$result))
      expect_false(is.na(x$result))
      expect_true(length(x$result) == 1)
    }
  )
})

test_that("returns FALSE when both vectors are entirely 0", {
  residuals <- rep(0, 1)
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.23
  )
  expect_false(result)

  residuals <- rep(0, 18)
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.862
  )
  expect_false(result)

  residuals[14] <- NA_real_
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.862
  )
  expect_false(result)
})

test_that("returns FALSE when both vectors are entirely NA_real_", {
  residuals <- rep(NA_real_, 1)
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.51
  )
  expect_false(result)

  residuals <- rep(NA_real_, 75)
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.9
  )
  expect_false(result)
})

test_that("returns FALSE when both vectors are of length 0", {
  residuals <- numeric()
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.001
  )
  expect_false(result)

  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals,
    threshold = 0.66
  )
  expect_false(result)
})

test_that("returns TRUE when after_seasonality is a constant-valued vector", {
  residuals <- rnorm(n = 67)
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = rep(0.1, length(residuals)),
    threshold = 0.5
  )
  expect_true(result)

  residuals <- rexp(n = 40, rate = 1/10)
  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = rep(1000, length(residuals)),
    threshold = 0.5
  )
  expect_true(result)
})

test_that("returns TRUE when residuals are scaled based on threshold", {
  set.seed(64920)

  residuals <- rt(n = 14, df = 2) + 0.5
  threshold <- runif(n = 1, min = 0.01, max = 0.99)
  factor <- runif(n = 1, min = threshold, max = 1)

  residuals_after_seasonality <- (residuals - median(residuals)) *
    (1 - factor) +
    median(residuals)

  # plot(residuals)
  # lines(residuals_after_seasonality)

  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals_after_seasonality,
    threshold = threshold
  )

  expect_true(result)

  residuals <- rt(n = 67, df = 2) + 0.5
  threshold <- runif(n = 1, min = 0.01, max = 0.99)
  factor <- runif(n = 1, min = threshold, max = 1)

  residuals_after_seasonality <- (residuals - median(residuals)) *
    (1 - factor) +
    median(residuals)

  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals_after_seasonality,
    threshold = threshold
  )

  expect_true(result)

  residuals <- rt(n = 2, df = 2) + 0.5
  threshold <- runif(n = 1, min = 0.01, max = 0.99)
  factor <- runif(n = 1, min = threshold, max = 1)

  residuals_after_seasonality <- (residuals - median(residuals)) *
    (1 - factor) +
    median(residuals)

  result <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals_after_seasonality,
    threshold = threshold
  )

  expect_true(result)
})
