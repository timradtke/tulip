#
# Try:
# 1) Depending on whether we see "outlier-ish" values after standardization or
#    initialization, automatically choose "norm" or "cauchy"
# 2) For outlier-ish values, during state fitting, replace the outlier value by
#    the predicted value at that point so as to not excessively influence
#    impact level/trend/seasonality state. In comparison to, for example, BSTS,
#    the error in the state-space form of the exponential smoothing approach
#    enters all state components smoothed by the state component's parameter
#    instead of the observation only. Very large outliers can thereby still
#    impact the state excessively.
#    In contrast, when fitting sigma, the outlier should not be replaced so as
#    to capture the uncertainty in the observation equation.
#    (the same might then need to be done when drawing sample paths)


#' Fit a robust exponential smoothing model via grid search
#'
#' @export
fit <- function(y,
                m,
                family = c("norm", "cauchy", "nbinom")[1],
                loss = "mae_penalized",
                lambda = 0.5,
                init_states = NULL,
                param_grid = NULL) {

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
    init_states <- initialize_states(y = y, m = m)
  } else {
    checkmate::assert_names(x = names(init_states), subset.of = c("l", "b", "s"))
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
    param_grid = param_grid
  )

  fitted_likelihood <- fit_likelihood(
    y = fitted_states$y,
    y_hat = fitted_states$y_hat,
    m = m,
    lambda = lambda,
    family = family,
    param_grid = param_grid,
    l = fitted_states$l,
    b = fitted_states$b,
    s = fitted_states$s,
    l_init = fitted_states$l_init,
    b_init = fitted_states$b_init,
    s_init = fitted_states$s_init
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
      x = x,
      x_hat = x_hat,
      y = y,
      y_hat = fitted_states$y_hat[, opt_idx],
      sigma = fitted_likelihood$sigma[opt_idx],
      l = fitted_states$l[, opt_idx],
      b = fitted_states$b[, opt_idx],
      s = fitted_states$s[, opt_idx],
      l_init = fitted_states$l_init[, opt_idx],
      b_init = fitted_states$b_init[, opt_idx],
      s_init = fitted_states$s_init[, opt_idx],
      param_grid = param_grid[opt_idx, ],
      y_median = y_median,
      y_mad = y_mad,
      loglik = fitted_likelihood$loglik[opt_idx],
      loss = fitted_likelihood$loss[opt_idx],
      mae = fitted_likelihood$mae[opt_idx],
      penalty = fitted_likelihood$penalty[opt_idx],
      lambda = lambda,
      family = family,
      m = m,
      full = list(
        param_grid = param_grid,
        loglik = fitted_likelihood$loglik,
        loss = fitted_likelihood$loss,
        mae = fitted_likelihood$mae,
        penalty = fitted_likelihood$penalty,
        sigma = fitted_likelihood$sigma,
        l = fitted_states$l,
        b = fitted_states$b,
        s = fitted_states$s,
        l_init = fitted_states$l_init,
        b_init = fitted_states$b_init,
        s_init = fitted_states$s_init,
        y_hat = fitted_states$y_hat
      )
    )
  )
}
