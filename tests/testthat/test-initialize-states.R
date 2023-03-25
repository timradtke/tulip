expect_state_component <- function(x, len) {
  checkmate::expect_numeric(
    x = x, finite = TRUE, any.missing = FALSE, len = len, null.ok = FALSE
  )
}

test_that("Initialization of states fails if all observations are missing", {
  expect_error(
    initialize_states(y = rep(NA, 50), m = 12, method = "additive"),
    regexp = "entirely missing"
  )
})

test_that("Seasonal state can be initialized even if one period is always missing", {
  y <- cospi((1:50) / 6)

  # setting the peaks of the cosine curve to NA
  y[(1:4)*12] <- NA

  states <- initialize_states(y = y, m = 12, method = "additive")

  expect_false(anyNA(states$s))
  expect_false(anyNA(states$l))
  expect_false(anyNA(states$b))

  # since all observations for the twelvth period were NA, the season is
  # initialized close to 0
  expect_true(
    all(abs(states$s[-(1:12)][is.na(y)]) < 0.1)
  )

  # all other periods' seasonality is estimated well
  expect_true(mean(abs(y[1:12] - states$s[1:12]), na.rm = TRUE) < 0.1)
})

test_that("Valid states are returned for short time series when seasonality is suspected", {
  init_states <- initialize_states(
    y = rnorm(1),
    m = 12,
    method = "additive",
    s = NULL,
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = 12 + 1
  )

  init_states <- initialize_states(
    y = rnorm(2),
    m = 12,
    method = "additive",
    s = NULL,
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = 12 + 2
  )

  init_states <- initialize_states(
    y = rnorm(14),
    m = 12,
    method = "additive",
    s = NULL,
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = 12 + 14
  )
})

test_that("Valid states are returned for short time series when no seasonality is suspected", {
  m <- 1

  init_states <- initialize_states(
    y = rnorm(1),
    m = m,
    method = "additive",
    s = NULL,
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = m + 1
  )

  init_states <- initialize_states(
    y = rnorm(2),
    m = m,
    method = "additive",
    s = NULL,
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = m + 2
  )

  init_states <- initialize_states(
    y = rnorm(14),
    m = m,
    method = "additive",
    s = NULL,
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = m + 14
  )
})

test_that("Valid states are returned for short time series when seasonality is user-provided", {
  m <- 12

  init_states <- initialize_states(
    y = rnorm(1),
    m = m,
    method = "additive",
    s = rep(0, m + 1),
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = m + 1
  )

  init_states <- initialize_states(
    y = rnorm(2),
    m = m,
    method = "additive",
    s = rep(0, m + 2),
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = m + 2
  )

  init_states <- initialize_states(
    y = rnorm(14),
    m = m,
    method = "additive",
    s = rep(0.25, m + 14),
    seasonality_threshold = 0.5,
    init_window_length = 5
  )

  lapply(
    X = init_states[c("l", "b", "s")],
    FUN = expect_state_component,
    len = m + 14
  )
})

test_that("Fails when time series is of length 0", {
  expect_error(
    initialize_states(
      y = numeric(length = 0L),
      m = 1,
      method = "additive",
      s = NULL,
      seasonality_threshold = 0.5,
      init_window_length = 5
    )
  )

  expect_error(
    initialize_states(
      y = numeric(length = 0L),
      m = 1,
      method = "multiplicative",
      s = NULL,
      seasonality_threshold = 0.5,
      init_window_length = 5
    )
  )

  expect_error(
    initialize_states(
      y = numeric(length = 0L),
      m = 12,
      method = "additive",
      s = NULL,
      seasonality_threshold = 0.5,
      init_window_length = 5
    )
  )

  expect_error(
    initialize_states(
      y = numeric(length = 0L),
      m = 12,
      method = "multiplicative",
      s = NULL,
      seasonality_threshold = 0.5,
      init_window_length = 5
    )
  )
})
