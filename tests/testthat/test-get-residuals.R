
test_that("residuals plus fitted equals actual series when no NAs", {
  for (i in 1:100) {
    n <- ceiling(rexp(n = 1, rate = 1/100))
    x <- rnorm(n = n)
    fitted <- rnorm(n = n)

    residuals <- get_residuals(x = x, fitted = fitted)

    expect_equal(x, fitted + residuals)
  }
})

test_that("throws error when `x` and `fitted` have different length", {
  expect_error(get_residuals(x = 1, fitted = 1:2))
  expect_error(get_residuals(x = 1:2, fitted = 1))
  expect_error(get_residuals(x = 1:6, fitted = 1:7))
  expect_error(get_residuals(x = rnorm(100), fitted = rnorm(3481)))
})

test_that("throws error for `x` and `fitted` of length less than 1", {
  expect_error(get_residuals(x = numeric(), fitted = numeric()))
})

test_that("throws error for `x` NULL", {
  expect_error(get_residuals(x = NULL, fitted = NULL))
})

test_that("throws error for `x` being character", {
  expect_error(get_residuals(x = letters, fitted = letters))
})
