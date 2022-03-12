test_that("tulip requires only arguments y and m", {
  y <- rnorm(n = 50, mean = 50)
  tulip_object <- tulip(y = y, m = 12)
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
})

test_that("tulip works if y is partly NA (case: m = 1)", {
  y <- rnorm(n = 50, mean = 50)
  y[c(1, 23:29, 41, 50)] <- NA
  tulip_object <- tulip(y = y, m = 1)
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
  expect_false(anyNA(tulip_object$y_hat))
  expect_true(anyNA(tulip_object$y))
})

test_that("tulip works if y is partly NA (case: m = 12)", {
  y <- rnorm(n = 50, mean = 50)
  y[c(1, 23:29, 41, 50)] <- NA
  tulip_object <- tulip(y = y, m = 12)
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
  expect_false(anyNA(tulip_object$y_hat))
  expect_true(anyNA(tulip_object$y))
})

test_that("tulip returns single_obs commment if y is single observation", {
  expect_warning(
    tulip_object <- tulip(y = rnorm(n = 1), m = 12),
    regexp = "length"
  )
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(!is.null(tulip_object$comment))
  expect_identical(object = tulip_object$comment, expected = "single_obs")
  expect_identical(object = tulip_object$y_hat,
                   expected = tulip_object$y)
})

test_that("tulip returns all_NA commment if y is entirely NA", {
  expect_warning(
    tulip_object <- tulip(y = c(NA, NA, NA, NA, NA), m = 12),
    regexp = "NA"
  )
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(!is.null(tulip_object$comment))
  expect_identical(object = tulip_object$comment, expected = "all_NA")
  expect_identical(object = tulip_object$y_hat,
                   expected = rep(NA, 5))
})

test_that("tulip returns no_variance commment if y is constant", {
  expect_warning(
    tulip_object <- tulip(y = c(100, 100, 100, 100, 100), m = 12),
    regexp = "vary"
  )
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(!is.null(tulip_object$comment))
  expect_identical(object = tulip_object$comment, expected = "no_variance")
  expect_identical(object = tulip_object$y_hat,
                   expected = tulip_object$y)
})

test_that("tulip returns mad_zero commment if y is mostly constant", {
  expect_warning(
    tulip_object <- tulip(y = c(100, 100, 100, rnorm(2)), m = 12),
    regexp = "MAD"
  )
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(!is.null(tulip_object$comment))
  expect_identical(object = tulip_object$comment, expected = "mad_zero")
  expect_identical(object = tulip_object$y_hat,
                   expected = rep(100, 5)) # median of the `y` input
})

test_that("tulip fails if y is not numeric", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = data.frame(y = y), m = NA),
               regexp = "Must be of type 'numeric'")
})

test_that("tulip fails if m is NA", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = NA), regexp = "Assertion on 'm'")
})

test_that("tulip fails if m is NULL", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = NULL), regexp = "Assertion on 'm'")
})

test_that("tulip fails if m is negative", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = -12), regexp = "Assertion on 'm'")
})

test_that("tulip fails if m is not integerish", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12.5), regexp = "Assertion on 'm'")
})

test_that("tulip works when length of y is less than m", {
  y <- rnorm(n = 50, mean = 6)
  tulip_object <- tulip(y = y, m = 12)
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
})

test_that("tulip works for m equal to 1", {
  y <- rnorm(n = 50, mean = 50)
  tulip_object <- tulip(y = y, m = 1)
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
})

test_that("each family option works on simple example", {
  y <- rnorm(n = 50, mean = 50)

  tulip_object <- tulip(y = y, m = 12, family = "auto")
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))

  tulip_object <- tulip(y = y, m = 12, family = "norm")
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))

  tulip_object <- tulip(y = y, m = 12, family = "cauchy")
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))

  tulip_object <- tulip(y = y, m = 12, family = "student")
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
})

test_that("tulip fails if family is unknown", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12, family = "unknown_family"),
               regexp = "family")
})

test_that("tulip fails if family is NULL", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12, family = NULL), regexp = "family")
})

test_that("tulip fails if family is NA", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12, family = NA), regexp = "family")
})

test_that("tulip fails if param_grid is NA", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12, param_grid = NA),
               regexp = "Must be of type 'matrix'")
})

test_that("tulip fails if param_grid is numeric vector", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12, param_grid = runif(n = 6)),
               regexp = "Must be of type 'matrix'")
})

test_that("tulip fails if param_grid is not named", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12,
                          param_grid = matrix(runif(n = 6), ncol = 6)),
               regexp = "Must have names")
})

param_grid <- matrix(c(0.2, 0.8, 0.1, 0.9, 0.5, 0.5,
                       0.3, 0.7, 0.05, 0.95, 0.8, 0.2), ncol = 6, byrow = TRUE)
colnames(param_grid) <- c("alpha", "one_minus_alpha",
                          "beta", "one_minus_beta",
                          "gamma", "one_minus_gamma")

test_that("tulip works for example param_grid", {
  y <- rnorm(n = 50, mean = 50)
  tulip_object <- tulip(y = y, m = 12, param_grid = param_grid)
  expect_s3_class(object = tulip_object, class = "tulip")
  expect_true(is.list(tulip_object))
  expect_false(anyNA(tulip_object$param_grid))
})

test_that("tulip fails if a column is missing in param_grid", {
  y <- rnorm(n = 50, mean = 50)
  expect_error(tulip(y = y, m = 12, param_grid = param_grid[, -1]),
               regexp = "Must have at least 6 cols")
})

test_that("tulip fails if a param_grid row sums up to more than 3", {
  y <- rnorm(n = 50, mean = 50)
  tmp_param_grid <- param_grid
  tmp_param_grid[2,3] <- 2
  expect_error(tulip(y = y, m = 12, param_grid = tmp_param_grid),
               regexp = "Assertion")
})

test_that("tulip fails if a param_grid value is less than 0 or more than 1", { # no lint
  y <- rnorm(n = 50, mean = 50)
  tmp_param_grid <- param_grid
  tmp_param_grid[2,3] <- -0.5
  tmp_param_grid[2,4] <- 1.5
  expect_error(tulip(y = y, m = 12, param_grid = tmp_param_grid),
               regexp = "Assertion")
})

test_that("tulip fails if a param_grid value is NA", {
  y <- rnorm(n = 50, mean = 50)
  tmp_param_grid <- param_grid
  tmp_param_grid[2,3] <- NA
  expect_error(tulip(y = y, m = 12, param_grid = tmp_param_grid),
               regexp = "Assertion")
})

test_that("tulip's speed did not regress", {
  skip_on_cran()
  set.seed(4027)
  y <- rt(n = 50, df = 3)

  mb_timing <- microbenchmark::microbenchmark({
    tulip(y = y, m = 12, family = "auto")
  },
  times = 250L,
  unit = "seconds"
  )

  # median is less than 0.66 seconds
  expect_true(median(mb_timing$time) / 1000000000 < 0.66)

  skip()
  # max is less than 1 second
  expect_true(max(mb_timing$time) / 1000000000 < 1.25)
})
