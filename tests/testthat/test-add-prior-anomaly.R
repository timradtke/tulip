test_that("add_prior_anomaly returns a list with required objects", {
  priors <- add_prior_anomaly(
    prob = 0.01,
  )

  expect_identical(
    object = priors,
    expected = list(
      anomaly = list(
        prob = 0.01
      )
    )
  )
})

test_that("add_prior_anomaly adds 'anomaly' list to existing list", {
  priors <- list(
    any_name_is_fine = list("something"),
    another_object = list("object")
  )

  priors <- add_prior_anomaly(
    prob = 0.01,
    priors = priors
  )

  expect_identical(
    object = priors,
    expected = list(
      any_name_is_fine = list("something"),
      another_object = list("object"),
      anomaly = list(
        prob = 0.01
      )
    )
  )
})

test_that("add_prior_anomaly fails when input is wrong", {
  expect_error(
    add_prior_anomaly(
      prob = -1
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_anomaly(
      prob = 2.5
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_anomaly(
      prob = c(0.5, 0.5)
    ),
    regexp = "prob"
  )

  expect_error(
    add_prior_anomaly(
      prob = 0.5,
      priors = data.frame(x = 1)
    ),
    regexp = "priors"
  )
})
