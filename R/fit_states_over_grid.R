

fit_states_over_grid <- function(y,
                                 m,
                                 init_states,
                                 param_grid,
                                 remove_anomalies,
                                 anomaly_candidates,
                                 anomaly_budget,
                                 anomaly_budget_most_recent_k,
                                 min_obs_anomaly_removal) {

  n <- length(y)
  k <- dim(param_grid)[1] # number of parameter combinations to trial

  # Initialize matrices that will be updated and store states ----

  anomaly_budget <- rep(anomaly_budget, k)

  # pad y with NA start values, then expand by number of parameter options
  y <- matrix(c(rep(NA, m), y), nrow = m + n, ncol = k, byrow = FALSE)

  # used to store the state predictions from all parameter combinations
  y_hat <- matrix(NA, nrow = m + n, ncol = k)

  # copy of `y` that will not be touched; used to return original `y` in the end
  y_orig <- y

  # copy of `y` that will keep separate account of observations that are
  # otherwise "cleaned" due to anomalies or missing values; can be used as
  # input to anomaly detection without creating feedback loop
  y_na <- y

  # expand states by number of parameter options
  if (length(init_states$l) != m + n) stop("l wrong length")
  if (length(init_states$b) != m + n) stop("b wrong length")
  if (length(init_states$s) != m + n) stop("s wrong length")

  l <- matrix(init_states$l, nrow = m + n, ncol = k, byrow = FALSE)
  b <- matrix(init_states$b, nrow = m + n, ncol = k, byrow = FALSE)
  s <- matrix(init_states$s, nrow = m + n, ncol = k, byrow = FALSE)

  # drop states entirely if very small for some reason
  b[, param_grid[, "beta"] + param_grid[, "one_minus_beta"] < 0.00001] <- 0
  s[, param_grid[, "gamma"] + param_grid[, "one_minus_gamma"] < 0.00001] <- 0

  # Iterate over all observations to update states for each parameter combo ----
  # All computations are applied in vectorized form across all parameter combos

  for (i in (m + 1):(m + n)) {

    # Update prediction at current observation ----

    y_hat[i, ] <- update_prediction(
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ]
    )

    # Release final anomaly budget ----

    if ((i - m) == (n + 1 - anomaly_budget_most_recent_k)) {
      # as there are only `anomaly_budget_most_recent_k` iterations left
      # including the current one, release the budget for the most recent k obs
      anomaly_budget <- anomaly_budget + anomaly_budget_most_recent_k
    }

    # Treat anomaly / missing value at current observation ----

    if (all(is.na(y[i,]))) {
      # treat missing values like anomalies but irrespective of anomaly budget
      y[i,] <- y_hat[i,]
      y_na[i,] <- NA
    } else if (isTRUE(remove_anomalies &&
                      i > (m + min_obs_anomaly_removal) &&
                      isTRUE(anomaly_candidates[i - m]) &&
                      any(anomaly_budget > 0))) {

      # overwrite actuals in case of outliers given errors so far;
      # start only when there are some errors available to compute the variance

      is_anomaly <- classify_anomaly(
        y = y_na[1:i, , drop = FALSE],
        y_hat = y_hat[1:i, , drop = FALSE],
        threshold = 3
      )

      y[i, ] <- overwrite_y_for_state_update(
        y = y[i, ],
        y_hat = y_hat[i, ],
        is_anomaly = is_anomaly,
        anomaly_budget = anomaly_budget
      )

      y_na[i, ] <- overwrite_y_for_sigma_update(
        y = y[i, ],
        y_na = y_na[i, ],
        is_anomaly = is_anomaly,
        anomaly_budget = anomaly_budget
      )

      # don't let anomaly budget drop below zero to allow for
      # `anomaly_budget_most_recent_k` update of budget at the end
      anomaly_budget <- pmax(anomaly_budget - is_anomaly, 0)
    }

    # Update states at current observation ----

    l[i, ] <- update_level(
      alpha = param_grid[, "alpha"],
      one_minus_alpha = param_grid[, "one_minus_alpha"],
      y = y[i, ],
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ]
    )

    b[i, ] <- update_trend(
      beta = param_grid[, "beta"],
      one_minus_beta = param_grid[, "one_minus_beta"],
      level_current = l[i, ],
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ]
    )

    s[i, ] <- update_season(
      gamma = param_grid[, "gamma"],
      one_minus_gamma = param_grid[, "one_minus_gamma"],
      y = y[i, ],
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ]
    )
  }

  # identify observations that were treated for missing values / outliers
  y_cleaned_mask <- is.na(y_na) | (y != y_orig)

  return(
    list(
      y_hat = y_hat[-(1:m), , drop = FALSE],
      y = y_orig[-(1:m), , drop = FALSE],
      y_cleaned = y[-(1:m), , drop = FALSE],
      y_na = y_na[-(1:m), , drop = FALSE],
      n_cleaned = colSums(y_cleaned_mask[-(1:m), , drop = FALSE]),
      e = y_hat[-(1:m), ] - y_orig[-(1:m), , drop = FALSE],
      l = l[-(1:m), , drop = FALSE],
      b = b[-(1:m), , drop = FALSE],
      s = s[-(1:m), , drop = FALSE],
      l_init = l[1:m, , drop = FALSE],
      b_init = b[1:m, , drop = FALSE],
      s_init = s[1:m, , drop = FALSE]
    )
  )
}

# provide
classify_anomaly <- function(y, y_hat, threshold = 3) {
  n_obs <- nrow(y)
  if (n_obs != nrow(y_hat)) {
    stop("Number of rows of `y` does not equal number of rows of `y_hat`.")
  }
  if (ncol(y) != ncol(y_hat)) {
    stop("Number of columns of `y` does not equal number of columns of `y_hat`.")
  }

  tmp_residuals <- y_hat[1:(n_obs - 1), , drop = FALSE] -
    y_na[1:(n_obs - 1), , drop = FALSE]

  tmp_sigma <- apply(
    tmp_residuals,
    MARGIN = 2,
    FUN = mad,
    na.rm = TRUE
  )

  current_error <- abs(as.numeric(y_hat[n_obs, ] - y[n_obs, ]))

  is_anomaly <- current_error > threshold * tmp_sigma
  is_anomaly <- ifelse(is.na(is_anomaly), FALSE, is_anomaly)

  if (length(is_anomaly) != ncol(y)) {
    stop("Length of `is_anomaly` would not be equal to number of columns of `y`.")
  }

  return(is_anomaly)
}

overwrite_y_for_state_update <- function(y, y_hat, is_anomaly, anomaly_budget) {
  checkmate::assert_numeric(x = y)
  checkmate::assert_numeric(x = y_hat, len = length(y))
  checkmate::assert_logical(x = is_anomaly, len = length(y))
  checkmate::assert_integerish(x = anomaly_budget, len = length(y))

  # this is equivalent to outlier treatment, and essentially "freezes"
  # the parameter update by pretending that the prediction was perfect

  y <- ifelse(
    is_anomaly & anomaly_budget > 0,
    y_hat,
    y
  )

  return(y)
}

overwrite_y_for_sigma_update <- function(y, y_na, is_anomaly, anomaly_budget) {
  checkmate::assert_numeric(x = y)
  checkmate::assert_numeric(x = y_na, len = length(y))
  checkmate::assert_logical(x = is_anomaly, len = length(y))
  checkmate::assert_integerish(x = anomaly_budget, len = length(y))

  # as `y_na` is used to create residuals for the outlier detection,
  # don't overwrite it but keep actuals including outliers to eventually
  # adjust the sigma used to detect outliers if there are tons

  return(y_na)
}

update_prediction <- function(level_previous,
                              trend_previous,
                              season_previous) {

  prediction_next <- level_previous + trend_previous + season_previous

  return(prediction_next)
}

update_level <- function(alpha,
                         one_minus_alpha,
                         y,
                         level_previous,
                         trend_previous,
                         season_previous) {
  level_next <- alpha * (y - season_previous) +
    one_minus_alpha * (level_previous + trend_previous)

  return(level_next)
}

update_trend <- function(beta,
                         one_minus_beta,
                         level_current,
                         level_previous,
                         trend_previous) {
  trend_next <- beta * (level_current - level_previous) +
    one_minus_beta * trend_previous

  return(trend_next)
}

update_season <- function(gamma,
                          one_minus_gamma,
                          y,
                          level_previous,
                          trend_previous,
                          season_previous) {

  season_next <- gamma * (y - level_previous - trend_previous) +
    one_minus_gamma * season_previous

  return(season_next)
}


