
n_obs <- ceiling(runif(n = 1, min = 0.1, max = 50))
y <- rnorm(n = n_obs, mean = rnorm(n = 1, sd = 1000), rexp(n = 1, rate = 1/100))
m <- sample(x = c(1, 2, 3, 6, 7, 12, 28, 60, 365), size = 1)

init_states <- list(
  l = rep(rnorm(n = 1), times = m + n_obs),
  b = rep(rnorm(n = 1), times = m + n_obs),
  s = rep(rnorm(n = m), times = 1 + ceiling(n_obs / m))[1:(m + n_obs)]
)

one_minus_alpha <- 1
one_minus_beta <- sample(x = c(0, 1), size = 1)
one_minus_gamma <- sample(x = c(0, 1), size = 1)

test_that("prediction is equal to initial states combination when smoothers are zero", {
  param_grid <- as.matrix(data.frame(
    alpha = 0, one_minus_alpha = one_minus_alpha,
    beta = 0, one_minus_beta = one_minus_beta,
    gamma = 0, one_minus_gamma = one_minus_gamma
  ))

  fitted <- fit_states_over_grid(
    y = y,
    m = m,
    init_states = init_states,
    param_grid = param_grid,
    remove_anomalies = FALSE,
    anomaly_candidates = 0,
    anomaly_budget = 0,
    anomaly_budget_most_recent_k = 0,
    min_obs_anomaly_removal = 2 * m
  )

  expect_equal(
    as.numeric(fitted$y_hat),
    init_states$l[-(1:m)] * one_minus_alpha +
      init_states$b[-(1:m)] * (1:n_obs) * one_minus_beta +
      init_states$s[-(1:m)] * one_minus_gamma
  )
})

test_that("random walk is possible based on param_grid choice", {
  param_grid <- as.matrix(data.frame(
    alpha = 1, one_minus_alpha = 0,
    beta = 0, one_minus_beta = 0,
    gamma = 0, one_minus_gamma = 0
  ))

  fitted <- fit_states_over_grid(
    y = y,
    m = m,
    init_states = init_states,
    param_grid = param_grid,
    remove_anomalies = FALSE,
    anomaly_candidates = 0,
    anomaly_budget = 0,
    anomaly_budget_most_recent_k = 0,
    min_obs_anomaly_removal = 2 * m
  )

  expect_equal(
    as.numeric(fitted$y_hat)[-1],
    y[-length(y)]
  )
})

test_that("seasonal random walk forecast is possible based on param_grid choice", {
  if (n_obs > m + 1) {
    param_grid <- as.matrix(data.frame(
      alpha = 0, one_minus_alpha = 0,
      beta = 0, one_minus_beta = 0,
      gamma = 1, one_minus_gamma = 0
    ))

    fitted <- fit_states_over_grid(
      y = y,
      m = m,
      init_states = init_states,
      param_grid = param_grid,
      remove_anomalies = FALSE,
      anomaly_candidates = 0,
      anomaly_budget = 0,
      anomaly_budget_most_recent_k = 0,
      min_obs_anomaly_removal = 2 * m
    )

    expect_equal(
      # need to drop the 'm+1"th observation from comparison as the prediction
      # still contains part of the level at that point
      as.numeric(fitted$y_hat)[-(1:(m+1))],
      y[-((length(y) - m + 1):length(y))][-1]
    )
  }
})







