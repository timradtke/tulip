test_that("add_prior_level returns a list with required objects", {
  priors <- add_prior_level(
    alpha = 1,
    beta = 2,
    verbose = FALSE,
    plot = FALSE
  )

  expect_identical(
    object = priors,
    expected = list(
      level = list(
        alpha = 1,
        beta = 2
      )
    )
  )
})

test_that("add_prior_level returns explainer when verbose is TRUE", {
  expect_message(
    add_prior_level(
      alpha = 1,
      beta = 2,
      verbose = TRUE,
      plot = FALSE
    ),
    regexp = "median at"
  )
})

test_that("add_prior_level adds 'level' list to existing list", {
  priors <- list(
    any_name_is_fine = list("something"),
    another_object = list("object")
  )

  priors <- add_prior_level(
    alpha = 1,
    beta = 2,
    priors = priors,
    verbose = FALSE,
    plot = FALSE
  )

  expect_identical(
    object = priors,
    expected = list(
      any_name_is_fine = list("something"),
      another_object = list("object"),
      level = list(
        alpha = 1,
        beta = 2
      )
    )
  )
})

test_that("add_prior_level fails when input is wrong", {
  expect_error(
    add_prior_level(
      alpha = 0,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_level(
      alpha = 1,
      beta = 0,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )

  expect_error(
    add_prior_level(
      alpha = c(1, 2),
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_level(
      alpha = 1,
      beta = c(1, 2),
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )

  expect_error(
    add_prior_level(
      alpha = -1,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_level(
      alpha = 1,
      beta = -2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )

  expect_error(
    add_prior_level(
      alpha = NA,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_level(
      alpha = 1,
      beta = NA,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )
})
