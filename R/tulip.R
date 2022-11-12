#' Fit a robust exponential smoothing model via grid search
#'
#' @param y A time series as numeric vector, may include NAs for some of the
#'   observations
#' @param m The time series' period length in number of observations (e.g., 12
#'   for yearly seasonality with monthly observations, 1 for no seasonality, ...
#'   ); does not handle multiple seasonality (e.g. weekly and yearly for daily
#'   observations)
#' @param family The distribution used to describe the error component; must be
#'   one of `auto` (default), `norm`, `student`, or `cauchy`.
#' @param param_grid Matrix defining the grid of parameters to be trialled
#'   during grid search optimization; default parameter grid will be used if
#'   left `NULL`. Can be created using [initialize_param_grid()].
#' @param priors List of priors on the models parameters; default priors will be
#'   used if left NULL. Can be created using [add_prior_error()],
#'   [add_prior_level()], [add_prior_seasonality()], [add_prior_trend()],
#'   [add_prior_anomaly()]
#' @param init_states List of initial states `l`, `b`, and `s` used to start
#'   iterative smoothing of the time series for each set of the parameters in
#'   the parameter grid. Default initialization via [initialize_states()] will
#'   be used if left `NULL`.
#' @param remove_anomalies Logical; during fitting, anomalies can be identified
#'   and interpolated to not adversely affect the fitted states. The
#'   interpolated values are only used to fit the states. When it comes to
#'   estimating the error's standard deviation `sigma` and to measuring the
#'   likelihood, interpolated values (i.e., fitted values) are compared against
#'   the original "anomalies" so that robust distributions like Cauchy and
#'   Student are correctly attributed a higher likelihood than the Normal
#'   distribution and may be chosen to project the uncertainty due to anomalies
#'   into the future. Default is `TRUE`.
#' @param anomaly_budget Integerish (default 5); the number of anomalies that
#'   can be interpolated during fitting of state components. It can be useful to
#'   set a somewhat low hard limit on the number of possible interpolations as
#'   some parameter grid combinations may be misspecified compared to the
#'   "correct" data generating process and would therefore consider every
#'   observation an anomaly.
#' @param anomaly_budget_most_recent_k Integerish (default 1); additional budget
#'   reserved to remove anomalies from the `k` most recent observations only.
#'   Especially models with larger `alpha` and `beta` smoothing parameters
#'   adjust the forecast strongly to the most recent observation(s). But even if
#'   the smoothing parameters are small, an anomalous most recent observation(s)
#'   can have a large impact on the forecast if it is sufficiently large (or
#'   small). At the same time, it can be undesired that the forecast adjusts
#'   heavily to the one, two, or k most recent observations even if they are
#'   very different from the rest of the series. For a discussion of this, see
#'   also: Michael Bohlke-Schneider, Shubham Kapoor, Tim Januschowski (2020).
#'   *Resilient Neural Forecasting Systems*.
#'   <https://www.amazon.science/publications/resilient-neural-forecasting-systems>
#' @param min_obs_anomaly_removal Integerish (default 12); the anomaly detection
#'   relies on the fitted values' errors' standard deviation. The standard
#'   deviation is iteratively updated as the state components are fitted from
#'   the first observation to the last observation. This parameter defines after
#'   which observation of the time series there are sufficiently many
#'   observations available to reliably estimate the error's standard deviation
#'   and thus determine whether an observation should be considered an anomaly.
#'   The default of 12 is useful for monthly observations and not too low. But
#'   it also implies that any anomaly in the first 12 observations will have
#'   an impact on the estimated state components.
#'
#' @return An object of class `tulip`, a list with components:
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
#' ls_fit <- tulip(y = y, m = 12, family = "norm")
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
tulip <- function(y,
                  m,
                  family = c("auto", "norm", "student", "cauchy")[1],
                  param_grid = NULL,
                  priors = NULL,
                  init_states = NULL,
                  seasonality_threshold = 0.5,
                  remove_anomalies = TRUE,
                  anomaly_budget = 5,
                  anomaly_budget_most_recent_k = 1,
                  min_obs_anomaly_removal = 12) {

  checkmate::assert_numeric(
    x = y, any.missing = TRUE, min.len = 1, null.ok = FALSE
  )
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
    x = anomaly_budget_most_recent_k, lower = 0, len = 1, any.missing = FALSE,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = min_obs_anomaly_removal, lower = 2, len = 1, any.missing = FALSE,
    null.ok = FALSE
  )

  x <- y
  n_y <- length(y)

  if (all(is.na(y))) {
    warning("All observations in `y` are NA. Returning NA as forecast.")
    return(default_object(y = y, y_hat = rep(NA, n_y), m = m,
                          comment = "all_NA"))
  }

  # Standardize Input Series ----

  # standardize the input time series to make grid search, common initial states
  # by learning across time series easier
  y_median <- median(x = y, na.rm = TRUE)
  y_mad <- mad(x = y, na.rm = TRUE)

  if (length(y) == 1) {
    warning("The length of the provided `y` is 1. Returning `y` as forecast.")
    return(default_object(y = y, y_hat = y, m = m, comment = "single_obs"))
  }
  if (isTRUE(sd(y) < 0.0001)) {
    warning("The provided `y` does not vary. Using `unique(y)` as forecast.")
    return(default_object(y = y, y_hat = y, m = m, comment = "no_variance"))
  }
  if (isTRUE(y_mad < 0.0001)) {
    warning("The MAD of y is 0, the series is constant for most observations. You might want to forecast it differently.") # nolint
    return(default_object(y = y, y_hat = rep(y_median, n_y), m = m,
                          comment = "mad_zero"))
  }

  y <- (y - y_median) / y_mad

  # Initialize States of the Model ----

  if (is.null(init_states)) {
    init_states <- initialize_states(
      y = y,
      m = m,
      seasonality_threshold = seasonality_threshold
    )
  } else {
    checkmate::assert_names(
      x = names(init_states),
      subset.of = c("l", "b", "s")
    )
  }

  # Initialize Parameter Grid to Optimize Over ----

  if (is.null(param_grid)) {
    param_grid <- initialize_param_grid()
  } else {
    checkmate::assert_names(
      x = colnames(param_grid),
      identical.to = c("alpha", "one_minus_alpha",
                       "beta", "one_minus_beta",
                       "gamma", "one_minus_gamma")
    )
    checkmate::assert_true(all((rowSums(param_grid) - 3) < 0.001))
    checkmate::assert_true(all(param_grid <= 1 & param_grid >= 0))
  }

  # Initialize Prior Distributions of Parameters ----

  if (is.null(priors)) {
    priors <- add_prior_error(guess = 0.1, n = 1)
    priors <- add_prior_anomaly(prob = 1 / n_y, priors = priors)
    priors <- add_prior_level(guess = 1/8, n = 8, priors = priors)
    priors <- add_prior_trend(
      prob = 0.75, guess = 1/15, n = 15, priors = priors
    )
    priors <- add_prior_seasonality(
      prob = 0.75, guess = 1/6, n = 6, priors = priors
    )
  } else {
    checkmate::assert_names(
      x = names(priors),
      must.include = c("error", "level", "trend", "seasonality")
    )
  }

  # Fit States Over the Parameter Grid ----

  fitted_states <- fit_states_over_grid(
    y = y,
    m = m,
    init_states = init_states,
    param_grid = param_grid,
    remove_anomalies = remove_anomalies,
    anomaly_candidates = init_states$anomaly_candidates,
    anomaly_budget = anomaly_budget,
    anomaly_budget_most_recent_k = anomaly_budget_most_recent_k,
    min_obs_anomaly_removal = min_obs_anomaly_removal
  )

  # Find the Optimal Set of Parameters ----

  fitted_map <- fit_map(
    param_grid = param_grid,
    priors = priors,
    family = family,
    m = m,
    y = fitted_states$y,
    y_hat = fitted_states$y_hat,
    n_cleaned = fitted_states$n_cleaned
  )

  ls_opt_idx <- fn_get_optimal_param_idx(
    log_joint = fitted_map$log_joint,
    idx_wrap = fitted_map$idx_wrap
  )

  # Back-transform the Input Series ----

  x_hat <- fitted_states$y_hat[, ls_opt_idx$wrapped] * y_mad + y_median

  # Return Results ----

  fitted_model <- list(
    y_hat = x_hat,
    y = x,
    x = y,
    x_hat = fitted_states$y_hat[, ls_opt_idx$wrapped],
    x_cleaned = fitted_states$y_cleaned[, ls_opt_idx$wrapped],
    n_cleaned = fitted_states$n_cleaned[ls_opt_idx$wrapped],
    x_na = fitted_states$y_na[, ls_opt_idx$wrapped],
    param_grid = param_grid[ls_opt_idx$wrapped, ],
    sigma = fitted_map$sigma[ls_opt_idx$raw],
    l = fitted_states$l[, ls_opt_idx$wrapped],
    b = fitted_states$b[, ls_opt_idx$wrapped],
    s = fitted_states$s[, ls_opt_idx$wrapped],
    l_init = fitted_states$l_init[, ls_opt_idx$wrapped],
    b_init = fitted_states$b_init[, ls_opt_idx$wrapped],
    s_init = fitted_states$s_init[, ls_opt_idx$wrapped],
    shift_range = init_states$shift_range,
    y_median = y_median,
    y_mad = y_mad,
    log_joint = fitted_map$log_joint[ls_opt_idx$raw],
    family = fitted_map$family[ls_opt_idx$raw],
    m = m,
    comment = NA_character_,
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

  class(fitted_model) <- "tulip"
  return(fitted_model)
}

#' Return an empty `tulip` object
#'
#' @param y A time series as numeric vector, may include NAs for some of the
#'   observations
#' @param y_hat Numeric vector of fitted values
#' @param m The time series' period length in number of observations (e.g., 12
#'   for yearly seasonality with monthly observations, 1 for no seasonality, ...
#'   ); does not handle multiple seasonality (e.g. weekly and yearly for daily
#'   observations)
#' @param comment A string that can be used to describe non-standard return
#'   cases
#'
default_object <- function(y, y_hat, m, comment) {
  checkmate::assert_numeric(x = y, null.ok = FALSE)
  checkmate::assert_numeric(x = y_hat, null.ok = FALSE)
  checkmate::assert_integerish(
    x = m, len = 1, lower = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_string(x = comment, na.ok = TRUE, null.ok = FALSE)

  param_grid <- matrix(NA, ncol = 6, nrow = 1)
  colnames(param_grid) <- c(
    "alpha", "one_minus_alpha",
    "beta", "one_minus_beta",
    "gamma", "one_minus_gamma"
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
  class(res) <- "tulip"

  return(res)
}

#' Get the index of parameters that optimize the log joint distribution
#'
#' Since we allow multiple `family`s to be evaluated at once with the same
#' grid of parameter values, we first need to identify the 'raw' index that
#' tells us the optimal `family` choice. Then we can within this family identify
#' the 'wrapped' optimum index that gives the optimal `params` choice.
#'
#' When only a single `family` is evaluated, the 'raw' and 'wrapped' indices are
#' the same.
#'
#' @param log_joint A vector of length `dim(param_grid)[1]` multiplied by the
#'     number of evaluated `family`s
#' @param idx_wrap The number of params (same for each `family`); used to
#'     indicate at which index the `log_joint` repeats the parameters
#'     for the next `family`
#'
#' @return A list of two scalar integers, named `raw` and `wrapped`
#'
#' @examples
#' # let's say there are 5 parameter sets being evaluated
#' log_joint_params <- rnorm(n = 5, sd = 5)
#' idx_wrap <- length(log_joint_params)
#'
#' # we evaluate the parameter sets for three different family choices, which
#' # creates 3 * 5 different sets of models to be evaluated
#' log_joint <- c(
#'   log_joint_params + rnorm(n = 1, sd = 5),
#'   log_joint_params + rnorm(n = 1, sd = 5),
#'   log_joint_params + rnorm(n = 1, sd = 5)
#' )
#'
#' ls_opt_idx <- fn_get_optimal_param_idx(
#'   log_joint = log_joint,
#'   idx_wrap = idx_wrap
#' )
#'
#' plot(x = 1:idx_wrap,
#'      y = seq(-30, 30, length.out = idx_wrap),
#'      type = "n", xlab = "idx", ylab = "log joint")
#'
#' for (i in 1:(length(log_joint) / idx_wrap)) {
#'   lines(
#'     x = 1:idx_wrap,
#'     y = log_joint[(i*idx_wrap - 4):(i*idx_wrap)],
#'     lty = i
#'   )
#' }
#'
#' points(ls_opt_idx$wrapped, log_joint[ls_opt_idx$raw], pch = 19)
#'
fn_get_optimal_param_idx <- function(log_joint, idx_wrap) {
  checkmate::assert_numeric(
    x = log_joint, min.len = 0, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = idx_wrap, len = 1, lower = 1, any.missing = FALSE, null.ok = FALSE
  )

  if (length(log_joint) == 0) {
    # this might occur even if the user specification is valid-ish
    stop("Failed to optimize the joint distribution. Does your time series include too many NAs, or did you provide an empty `param_grid`?") # nolint
  }
  if (length(log_joint) %% idx_wrap != 0) {
    # this should only occur if the package-internal code is misspecified
    stop("`log_joint` must be a vector of length that is a multiple of `idx_wrap`.")
  }

  opt_idx <- which.max(log_joint)

  if (opt_idx > idx_wrap) {
    opt_idx_wrapped <- opt_idx %% idx_wrap
    if (opt_idx_wrapped == 0) {
      opt_idx_wrapped <- idx_wrap
    }
  } else {
    opt_idx_wrapped <- opt_idx
  }

  return(list(raw = opt_idx, wrapped = opt_idx_wrapped))
}
