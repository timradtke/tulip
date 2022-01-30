#' Fit a robust exponential smoothing model via grid search
#'
#' @export
fit <- function(y,
                m,
                family = c("norm", "cauchy", "nbinom")[1],
                loss = "mae_penalized",
                lambda = 0.5,
                lambda_outlier = 10,
                init_states = NULL,
                param_grid = NULL,
                shift_detection = TRUE,
                window_size = 5,
                remove_outliers = TRUE,
                outlier_budget = 5,
                min_obs_for_outlier_sigma = 10,
                seasonality_threshold = 0.5,
                verbose = TRUE) {

  checkmate::assert_numeric(x = y, any.missing = FALSE)
  checkmate::assert_integerish(
    x = m, any.missing = FALSE, null.ok = FALSE, len = 1
  )
  checkmate::assert_choice(
    x = family, choices = c("norm", "cauchy", "nbinom"), null.ok = FALSE
  )
  checkmate::assert_list(
    x = init_states, types = c("numeric"), any.missing = FALSE, null.ok = TRUE
  )
  checkmate::assert_matrix(
    x = param_grid, any.missing = FALSE, min.rows = 1, min.cols = 6,
    max.cols = 6, null.ok = TRUE
  )

  # create a copy of y
  x <- y
  n_y <- length(y)

  # standardize the input time series to make grid search, common initial states
  # by learning across time series easier
  if (family %in% c("norm", "cauchy")) {
    y_median <- median(x = y)
    y_mad <- mad(x = y)
    y <- (y - y_median) / y_mad
  } else if (family == "nbinom") {
    y_median <- NA
    y_mad <- NA
    y <- log1p(y)
  }

  if (is.null(init_states)) {
    init_states <- initialize_states(
      y = y,
      m = m,
      shift_detection = shift_detection,
      window_size = window_size,
      seasonality_threshold = seasonality_threshold
    )
  } else {
    checkmate::assert_names(
      x = names(init_states),
      subset.of = c("l", "b", "s")
    )
  }

  if (is.null(param_grid)) {
    param_grid <- initialize_param_grid()
  } else {
    checkmate::assert_names(
      x = colnames(param_grid),
      must.include = c("alpha", "beta", "gamma", "one_minus_alpha",
                       "one_minus_beta", "one_minus_gamma")
    )
    checkmate::assert_true(all((rowSums(param_grid) - 6) < 0.001))
  }

  fitted_states <- fit_states_over_grid(
    y = y,
    m = m,
    init_states = init_states,
    param_grid = param_grid,
    remove_outliers = remove_outliers,
    outlier_budget = outlier_budget,
    min_obs_for_outlier_sigma = min_obs_for_outlier_sigma
  )

  fitted_likelihood <- fit_likelihood(
    y = fitted_states$y,
    y_hat = fitted_states$y_hat,
    m = m,
    lambda = lambda,
    lambda_outlier = lambda_outlier,
    family = family,
    param_grid = param_grid,
    l = fitted_states$l,
    b = fitted_states$b,
    s = fitted_states$s,
    l_init = fitted_states$l_init,
    b_init = fitted_states$b_init,
    s_init = fitted_states$s_init,
    n_cleaned = fitted_states$n_cleaned
  )

  opt_idx <- which.min(fitted_likelihood$loss)

  # back-transform the fitted values
  if (family == "nbinom") {
    x_hat <- expm1(fitted_states$y_hat[, opt_idx])
  } else {
    x_hat <- fitted_states$y_hat[, opt_idx] * y_mad + y_median
  }

  return(
    list(
      y_hat = x_hat,
      y = x,
      x = y,
      x_hat = fitted_states$y_hat[, opt_idx],
      x_cleaned = fitted_states$y_cleaned[, opt_idx],
      n_cleaned = fitted_states$n_cleaned[opt_idx],
      x_na = fitted_states$y_na[, opt_idx],
      param_grid = param_grid[opt_idx, ],
      sigma = fitted_likelihood$sigma[opt_idx],
      l = fitted_states$l[, opt_idx],
      b = fitted_states$b[, opt_idx],
      s = fitted_states$s[, opt_idx],
      l_init = fitted_states$l_init[, opt_idx],
      b_init = fitted_states$b_init[, opt_idx],
      s_init = fitted_states$s_init[, opt_idx],
      shift_range = init_states$shift_range,
      y_median = y_median,
      y_mad = y_mad,
      loglik = fitted_likelihood$loglik[opt_idx],
      loss = fitted_likelihood$loss[opt_idx],
      mae = fitted_likelihood$mae[opt_idx],
      penalty = fitted_likelihood$penalty[opt_idx],
      penalty_outlier = fitted_likelihood$penalty_outlier[opt_idx],
      lambda = lambda,
      family = family,
      m = m,
      full = list(
        param_grid = param_grid,
        loglik = fitted_likelihood$loglik,
        loss = fitted_likelihood$loss,
        mae = fitted_likelihood$mae,
        penalty = fitted_likelihood$penalty,
        penalty_outlier = fitted_likelihood$penalty_outlier,
        sigma = fitted_likelihood$sigma,
        l = fitted_states$l,
        b = fitted_states$b,
        s = fitted_states$s,
        l_init = fitted_states$l_init,
        b_init = fitted_states$b_init,
        s_init = fitted_states$s_init,
        x_hat = fitted_states$y_hat,
        x_cleaned = fitted_states$y_cleaned,
        n_cleaned = fitted_states$n_cleaned
      )
    )
  )
}
