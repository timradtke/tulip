test_that("add_prior_trend returns a list with required objects", {
  priors <- add_prior_trend(
    prob = 0.5,
    alpha = 1,
    beta = 2,
    verbose = FALSE,
    plot = FALSE
  )

  expect_identical(
    object = priors,
    expected = list(
      trend = list(
        prob = 0.5,
        alpha = 1,
        beta = 2
      )
    )
  )
})

test_that("add_prior_trend returns explainer when verbose is TRUE", {
  suppressMessages(
    expect_message(
      add_prior_trend(
        prob = 0.5,
        alpha = 1,
        beta = 2,
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
      trend = list(
        prob = 0.5,
        alpha = 1,
        beta = 2
      )
    )
  )
})

test_that("add_prior_trend fails when input is wrong", {
  expect_error(
    add_prior_trend(
      prob = 2,
      alpha = 1,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      prob = -0.01,
      alpha = 1,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = 0,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_trend(
      prob = c(0.5, 0.5),
      alpha = 1,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = c(1, 2),
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = 1,
      beta = c(1, 2),
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = 1,
      beta = 0,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = -1,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = 1,
      beta = -2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )

  expect_error(
    add_prior_trend(
      prob = NA,
      alpha = 1,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = NA,
      beta = 2,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "alpha"
  )

  expect_error(
    add_prior_trend(
      prob = 0.5,
      alpha = 1,
      beta = NA,
      verbose = FALSE,
      plot = FALSE
    ),
    regexp = "beta"
  )
})
