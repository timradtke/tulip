test_that("add_prior_level returns a list with required objects", {
  priors <- add_prior_level(
    priors = NULL,
    guess = 0.25,
    n = 4,
    verbose = FALSE,
    plot = FALSE
  )

  expect_identical(
    object = priors,
    expected = list(
      level = list(
        alpha = 0.25 * 4,
        beta = 4 - 0.25 * 4
      )
    )
  )
})

test_that("add_prior_level returns explainer when verbose is TRUE", {
  expect_message(
    add_prior_level(
      priors = NULL,
      guess = 0.33,
      n = 3,
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
    guess = 0.25,
    n = 4,
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
        alpha = 0.25 * 4,
        beta = 4 - 0.25 * 4
      )
    )
  )
})

test_that("add_prior_level fails when input is wrong", {
  expect_error(
    add_prior_level(
      priors = NULL,
      guess = 0,
      n = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = 1,
      n = 0,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = c(1, 2),
      n = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = 1,
      n = c(1, 2),
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = -1,
      n = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = 1,
      n = -2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = NA,
      n = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_level(
      priors = NULL,
      guess = 1,
      n = NA,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )
})
