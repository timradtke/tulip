#' Fit parameters given time series, initial states, and parameter grid
#'
#' @param y Time series to which the model is fitted
#' @param m Scalar indicating the period length of the suspected seasonality
#' @param init_states A list of initial states as returned by
#'     `initialize_states()`
#' @param param_grid A matrix of possible parameter values used in grid search
#'
fit_states_over_grid <- function(y,
                                 m,
                                 init_states,
                                 param_grid,
                                 remove_outliers,
                                 outlier_budget,
                                 min_obs_for_outlier_sigma = 10) {

  n <- length(y)
  k <- dim(param_grid)[1] # number of parameter combinations to trial

  # pad y with NA start values, then expand by number of parameter options
  y <- matrix(c(rep(NA, m), y), nrow = m + n, ncol = k, byrow = FALSE)
  y_hat <- matrix(NA, nrow = m + n, ncol = k)

  # expand states by number of parameter options
  if (length(init_states$l) != m + n) stop("l wrong length")
  if (length(init_states$b) != m + n) stop("b wrong length")
  if (length(init_states$s) != m + n) stop("s wrong length")

  l <- matrix(init_states$l, nrow = m + n, ncol = k, byrow = FALSE)
  b <- matrix(init_states$b, nrow = m + n, ncol = k, byrow = FALSE)
  s <- matrix(init_states$s, nrow = m + n, ncol = k, byrow = FALSE)

  b[, param_grid[, "beta"] + param_grid[, "one_minus_beta"] < 0.00001] <- 0
  s[, param_grid[, "gamma"] + param_grid[, "one_minus_gamma"] < 0.00001] <- 0

  # for each time step, perform the same transformations for each set of
  # parameters in vectorized form

  y_orig <- y
  y_na <- y
  outlier_budget <- rep(outlier_budget, k)

  for (i in (m + 1):(m + n)) {
    y_hat[i, ] <- l[i-1, ] + b[i-1, ] + s[i-m, ]

    ############################################################################

    if (remove_outliers &&
        i > (m + min_obs_for_outlier_sigma) &&
        any(outlier_budget > 0)) {
      # overwrite actuals in case of outliers given errors so far;
      # start only when there are some errors available to compute the variance
      tmp_sigma <- sqrt(
        colMeans(x = (y_hat[1:(i-1), , drop = FALSE] -
                        y_na[1:(i-1), , drop = FALSE])^2, na.rm = TRUE) -
          colMeans(x = y_hat[1:(i-1), , drop = FALSE] -
                     y_na[1:(i-1), , drop = FALSE], na.rm = TRUE)^2
      )

      is_outlier <- pnorm(
        q = abs(as.numeric(y_hat[i,] - y[i,])),
        mean = 0,
        sd = tmp_sigma
      ) > 0.99

      outlier_budget <- outlier_budget - is_outlier

      # use `y_hat` instead of `y` to continue update of parameters
      y[i,] <- ifelse(is_outlier & outlier_budget > 0, y_hat[i,], y[i,])
      # use `NA` instead of `y` to continue update of `tmp_sigma` next round
      # (we can't use `y_hat` because this will make `tmp_sigma` converge to 0)
      y_na[i,] <- ifelse(is_outlier & outlier_budget > 0, NA, y[i,])
    }

    ############################################################################

    l[i, ] <- param_grid[, "alpha"] * (y[i, ] - s[i-m, ]) +
      param_grid[, "one_minus_alpha"] * (l[i-1, ] + b[i-1, ])

    b[i, ] <- param_grid[, "beta"] * (l[i, ] - l[i-1, ]) +
      param_grid[, "one_minus_beta"] * b[i-1, ]

    s[i, ] <- param_grid[, "gamma"] * (y[i, ] - l[i-1, ] - b[i-1, ]) +
      param_grid[, "one_minus_gamma"] * s[i-m, ]
  }

  return(
    list(
      y_hat = y_hat[-(1:m), , drop = FALSE],
      y = y_orig[-(1:m), , drop = FALSE],
      y_cleaned = y[-(1:m), , drop = FALSE],
      y_na = y_na[-(1:m), , drop = FALSE],
      n_cleaned = colSums(is.na(y_na[-(1:m), , drop = FALSE])),
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
