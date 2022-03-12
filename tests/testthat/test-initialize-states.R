test_that("Initialization of states fails if all observations are missing", {
  expect_error(
    initialize_states(y = rep(NA, 50), m = 12),
    regexp = "entirely missing"
  )
})

test_that("Seasonal state is 0 with warning if every m-th observation is missing", {
  y <- rnorm(n = 50)
  y[(1:4)*12] <- NA
  expect_warning({
    states <- initialize_states(y = y, m = 12)
  },
  regexp = "Cannot estimate seasonal initial state"
  )
  expect_true(all(states$s == 0))
  expect_false(anyNA(states$l))
  expect_false(anyNA(states$b))
})
