fit <- heuristika(y = rnorm(n = 50), m = 12)

test_that("predict.heuristika returns expected output in default case", {
  forecast <- predict(object = fit)

  expect_s3_class(object = forecast, class = c("heuristika_paths"))
  expect_identical(object = dim(forecast$paths), expected = c(12L, 10000L))
  expect_true(!anyNA(forecast$paths))
})

test_that("predict.heuristika fails when h is not larger than 0", {
  expect_error(predict(object = fit, h = 0))
  expect_error(predict(object = fit, h = -5))
})

test_that("predict.heuristika fails when h is not integerish", {
  expect_error(predict(object = fit, h = 6.5))
})

test_that("predict.heuristika fails when n is not larger than 0", {
  expect_error(predict(object = fit, n = 0))
  expect_error(predict(object = fit, n = -5))
})

test_that("predict.heuristika fails when n is not integerish", {
  expect_error(predict(object = fit, n = 6.5))
})

test_that("predict.heuristika returns expected output in default case", {
  set.seed(4027)
  # use t-distribution so that "anomalies" are created; force normal likelihood
  fit <- heuristika(y = rt(n = 50, df = 1), m = 12, family = "norm")

  set.seed(6582)
  forecast_without <- predict(object = fit, switch_to_cauchy_if_outliers = FALSE)

  set.seed(6582)
  forecast_with <- predict(object = fit, switch_to_cauchy_if_outliers = TRUE)

  # there must be a difference given that the samples are supposed to come
  # once from `rnorm()` and once from `rcauchy()`
  expect_false(identical(forecast_without$paths, forecast_with$paths))
})

test_that("predict.heuristika's speed did not regress", {
  skip_on_cran()

  mb_timing <- microbenchmark::microbenchmark({
    predict(object = fit)
  },
  times = 250L,
  unit = "seconds"
  )

  # median is less than 0.15 seconds
  expect_true(median(mb_timing$time) / 1000000000 < 0.15)

  # max is less than 0.33 seconds
  expect_true(max(mb_timing$time) / 1000000000 < 0.5)
})
