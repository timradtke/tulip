test_that("add_prior_trend returns a list with required objects", {
  priors <- add_prior_trend(
    priors = NULL,
    prob = 0.5,
    guess = 0.25,
    n = 4,
    verbose = FALSE,
    plot = FALSE
  )

  expect_identical(
    object = priors,
    expected = list(
      trend = list(
        prob = 0.5,
        alpha = 0.25 * 4,
        beta = 4 - 0.25 * 4
      )
    )
  )
})

test_that("add_prior_trend returns explainer when verbose is TRUE", {
  suppressMessages(
    expect_message(
      add_prior_trend(
        priors = NULL,
        prob = 0.5,
        guess = 0.25,
        n = 4,
        verbose = TRUE,
        plot = FALSE
      ),
      regexp = "median at"
    )
  )
})

test_that("add_prior_trend adds 'trend' list to existing list", {
  priors <- list(
    any_name_is_fine = list("something"),
    another_object = list("object")
  )

  priors <- add_prior_trend(
    prob = 0.5,
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
      trend = list(
        prob = 0.5,
        alpha = 0.25 * 4,
        beta = 4 - 0.25 * 4
      )
    )
  )
})

test_that("add_prior_trend fails when input is wrong", {
  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 2,
      guess = 0.25,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = -0.01,
      guess = 0.25,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = 0,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = c(0.5, 0.5),
      guess = 0.25,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = c(0.25, 0.44),
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = 0.25,
      n = c(1, 2),
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = 0.25,
      n = 0,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = -1,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = 0.25,
      n = -2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = NA,
      guess = 0.25,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = NA,
      n = 4,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "guess"
  )

  expect_error(
    add_prior_trend(
      priors = NULL,
      prob = 0.5,
      guess = 0.25,
      n = NA,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "n"
  )
})
