#' Fit a robust exponential smoothing model via grid search
#'
#' @return An object of class `heuristika`, a list with components:
#' \describe{
#'   \item{y_hat}{Fitted values}
#'   \item{y}{Input time series}
#'   \item{x_hat}{Fitted values on standardized scale}
#'   \item{x}{Input time series on standardized scale}
#' }
#'
#' @seealso [draw_paths()], [initialize_states()], [initialize_param_grid()],
#'     [add_prior_level()], [add_prior_trend()], [add_prior_seasonality()],
#'     [add_prior_error()], [add_prior_anomaly()]
#'
#' @export
#' @examples
#' set.seed(4278)
#' y <- rt(100, df = 10) * 10 + 1:100
#'
#' ls_fit <- heuristika(y = y, m = 12, family = "norm")
#'
#' print(ls_fit$family)
#' print(ls_fit$param_grid)
#'
#' plot(y, type = "l", col = "grey", xlab = NA)
#' points(y, pch = 21, bg = "black", col = "white")
#'
#' # add fitted values
#' lines(ls_fit$y_hat, col = "blue")
#'
#' # add actuals with interpolated anomalies
#' points(ls_fit$x_cleaned * ls_fit$y_mad + ls_fit$y_median,
#'        col = "orange", pch = 21)
#'
heuristika <- function(y,
                       m,
                       family = c("auto", "norm", "student", "cauchy")[1],
                       param_grid = NULL,
                       priors = NULL,
                       init_states = NULL,
                       seasonality_threshold = 0.5,
                       remove_anomalies = TRUE,
                       anomaly_budget = 5,
                       min_obs_anomaly_removal = 12) {

  checkmate::assert_numeric(x = y, any.missing = FALSE)
  checkmate::assert_integerish(
    x = m, any.missing = FALSE, null.ok = FALSE, len = 1, lower = 1
  )
  checkmate::assert_choice(
    x = family, choices = c("norm", "cauchy", "student", "auto"),
    null.ok = FALSE
  )
  checkmate::assert_matrix(
    x = param_grid,
    any.missing = FALSE, min.rows = 1, min.cols = 6,
    max.cols = 6, null.ok = TRUE
  )
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_list(
    x = init_states, types = c("numeric"), any.missing = FALSE, null.ok = TRUE
  )
  checkmate::assert_numeric(
    x = seasonality_threshold, lower = 0, upper = 1, len = 1,
    any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_logical(
    x = remove_anomalies, len = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = anomaly_budget, lower = 0, len = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = min_obs_anomaly_removal, lower = 2, len = 1, any.missing = FALSE,
    null.ok = FALSE
  )

  x <- y
  n_y <- length(y)

  # standardize the input time series to make grid search, common initial states
  # by learning across time series easier
  y_median <- median(x = y)
  y_mad <- mad(x = y)

  if (length(y) == 1) {
    warning("The length of the provided `y` is 1. Returning `y` as forecast.")
    return(default_object(y = y, y_hat = y, m = m, comment = "single_obs"))
  }
  if (isTRUE(sd(y) < 0.0001)) {
    warning("The provided `y` does not vary. Using `unique(y)` as forecast.")
    return(default_object(y = y, y_hat = y, m = m, comment = "no_variance"))
  }
  if (isTRUE(y_mad < 0.0001)) {
    warning("The MAD of y is 0, the series is constant for most observations. You might want to forecast it differently.") # no lint
    return(default_object(y = y, y_hat = rep(y_median, n_y), m = m,
                          comment = "mad_zero"))
  }

  y <- (y - y_median) / y_mad

  if (is.null(init_states)) {
    init_states <- initialize_states(
      y = y,
      m = m,
      shift_detection = FALSE, # hardcoded until no longer experimental
      window_size = 5, # hardcoded until no longer experimental
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

    # ensure the expected order of columns, just in case
    param_grid <- param_grid[, c("alpha", "one_minus_alpha",
                                 "beta", "one_minus_beta",
                                 "gamma", "one_minus_gamma")]
    checkmate::assert_true(all((rowSums(param_grid) - 3) < 0.001))
    checkmate::assert_true(all(param_grid <= 1 & param_grid >= 0))
  }

  if (is.null(priors)) {
    priors <- add_prior_error(shape = 3, rate = 2*0.75)
    priors <- add_prior_anomaly(prob = 1 / n_y, priors = priors)
    priors <- add_prior_level(alpha = 1, beta = 7, priors = priors)
    priors <- add_prior_trend(
      prob = 0.75, alpha = 1, beta = 14, priors = priors
    )
    priors <- add_prior_seasonality(
      prob = 0.75, alpha = 1, beta = 5, priors = priors
    )
  } else {
    checkmate::assert_names(
      x = names(priors),
      must.include = c("error", "level", "trend", "seasonality")
    )
  }

  fitted_states <- fit_states_over_grid(
    y = y,
    m = m,
    init_states = init_states,
    param_grid = param_grid,
    remove_anomalies = remove_anomalies,
    anomaly_budget = anomaly_budget,
    min_obs_anomaly_removal = min_obs_anomaly_removal
  )

  fitted_map <- fit_map(
    param_grid = param_grid,
    priors = priors,
    family = family,
    m = m,
    y = fitted_states$y,
    y_hat = fitted_states$y_hat,
    n_cleaned = fitted_states$n_cleaned
  )

  opt_idx <- which.max(fitted_map$log_joint)
  if (opt_idx > fitted_map$idx_wrap) {
    opt_idx_wrapped <- opt_idx %% fitted_map$idx_wrap
  } else {
    opt_idx_wrapped <- opt_idx
  }

  # back-transform the fitted values
  x_hat <- fitted_states$y_hat[, opt_idx_wrapped] * y_mad + y_median

  fitted_model <- list(
    y_hat = x_hat,
    y = x,
    x = y,
    x_hat = fitted_states$y_hat[, opt_idx_wrapped],
    x_cleaned = fitted_states$y_cleaned[, opt_idx_wrapped],
    n_cleaned = fitted_states$n_cleaned[opt_idx_wrapped],
    x_na = fitted_states$y_na[, opt_idx_wrapped],
    param_grid = param_grid[opt_idx_wrapped, ],
    sigma = fitted_map$sigma[opt_idx],
    l = fitted_states$l[, opt_idx_wrapped],
    b = fitted_states$b[, opt_idx_wrapped],
    s = fitted_states$s[, opt_idx_wrapped],
    l_init = fitted_states$l_init[, opt_idx_wrapped],
    b_init = fitted_states$b_init[, opt_idx_wrapped],
    s_init = fitted_states$s_init[, opt_idx_wrapped],
    shift_range = init_states$shift_range,
    y_median = y_median,
    y_mad = y_mad,
    log_joint = fitted_map$log_joint[opt_idx],
    family = fitted_map$family[opt_idx],
    m = m,
    comment = NA,
    full = list(
      param_grid = param_grid,
      log_joint = fitted_map$log_joint,
      family = fitted_map$family,
      sigma = fitted_map$sigma,
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

  class(fitted_model) <- "heuristika"
  return(fitted_model)
}

default_object <- function(y, y_hat, m, comment) {
  param_grid <- matrix(NA, ncol = 6, nrow = 1)
  colnames(param_grid) <- c(
    "alpha", "one_minus_alpha", "beta", "one_minus_beta", "gamma",
    "one_minus_gamma"
  )

  res <- list(
    y_hat = y_hat,
    y = y,
    x = y,
    x_hat = y_hat,
    x_cleaned = y,
    n_cleaned = 0,
    x_na = y,
    param_grid = param_grid,
    sigma = NA,
    l = NA,
    b = NA,
    s = NA,
    l_init = NA,
    b_init = NA,
    s_init = NA,
    shift_range = c(NA, NA),
    y_median = NA,
    y_mad = NA,
    log_joint = 0,
    family = "norm",
    m = m,
    comment = comment
  )

  res$full <- res
  class(res) <- "heuristika"

  return(res)
}
