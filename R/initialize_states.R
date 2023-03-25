#' Initialize state components given time series
#'
#' Perform a robust initialization of seasonal, local-linear trend, and level
#' components given a suspected period length and the time series.
#'
#' A crucial component when fitting exponential smoothing models is the
#' initialization of the model's state components (level, trend, seasonality).
#'
#' A poorly chosen initialization can prevent the model from adjusting neatly
#' to the data. At the same time, finding a good initialization can be hard as
#' one essentially has to already fit to the entire series to properly
#' distinguish impacts from trend and seasonality. Initialization for time
#' series is a chicken-and-egg problem. Standard methods can additionally be
#' susceptible to anomalies.
#'
#' While `initialize_states()` can be applied by a user of the `tulip` package
#' directly to find initial states to provide to the [tulip()] function's
#' `init_states` argument, it will also be called internally by [tulip()] if no
#' `init_states` are provided.
#'
#' Optionally, the seasonality component can be provided as `s`. The
#' initialization problem then reduces to identifying the level and trend
#' components *given* the seasonal component. This way, a seasonal component
#' can be transferred from a previously fitted, related time series, which is
#' especially comfortable when the seasonality component is chosen to be
#' multiplicative, via `method = "multiplicative"`.
#'
#' @param y Input time series to fit the model against
#' @param m Scalar indicating the period length of the potential seasonality
#' @param method Are we using an `additive` or a `multiplicative` seasonal
#'     component?
#' @param s An optional numeric vector representing the initial seasonal state
#'     component. If provided, the the seasonal component is not derived
#'     automatically. Instead, the user-provided seasonal state is used, and the
#'     components `l` and `b` are derived given the user-provided `s`.
#'     The provided `s` must be of length `m + length(y)`, where
#'     the first `m` values are the states ahead of the observed time period of
#'     the time series `y`.
#' @param seasonality_threshold A value between 0 and 1; a seasonal component is
#'     not used when the seasonal component's reduction of variance in the
#'     residuals is less than the threshold
#' @param init_window_length How many observations should be used to fit initial
#'     level and trend after the seasonal component was removed?
#'     Ideally, one uses few to let changes in state occur later via smoothing.
#'     But too few observations can lead to unrobust results.
#'
#' @seealso [tulip()]
#'
#' @references
#' \describe{
#'   \item{Ruben Crevits and Christophe Croux (2017). *Forecasting using Robust Exponential Smoothing with Damped Trend and Seasonal Components*.}{\url{https://dx.doi.org/10.2139/ssrn.3068634}}
#'   \item{Rob J. Hyndman, Anne B. Koehler, Ralph D. Snyder, and Simone Grose (2002). *A State Space Framework for Automatic Forecasting using Exponential Smoothing Methods*.}{\url{https://doi.org/10.1016/S0169-2070(01)00110-8}}
#'   \item{Rafael de Rezende, Katharina Egert, Ignacio Marin, Guilherme Thompson (2021). *A White-boxed ISSM Approach to Estimate Uncertainty Distributions of Walmart Sales*.}{\url{https://arxiv.org/abs/2111.14721}}
#' }
#'
#' @export
#' @examples
#' y <- as.numeric(AirPassengers[1:68])
#' init_state <- initialize_states(
#'   y = y, m = 12, method = "multiplicative", seasonality_threshold = 0.5
#' )
#'
#' plot(-11:length(y), c(rep(NA, 12), y), pch = 19, cex = 0.5)
#' lines(-11:length(y),
#'       (init_state$l + init_state$b*(-11:length(y))) * init_state$s)
#'
#' lines(1:length(y), init_state$fitted_global, col = "blue")
#' lines(1:length(init_state$fitted_local),
#'       init_state$fitted_local, col = "red")
#'
#' y <- tulip::flowers$flowers
#' init_state <- initialize_states(y = y, m = 12, method = "multiplicative")
#'
#' plot(-11:length(y), c(rep(NA, 12), y), pch = 19, cex = 0.5)
#' lines(-11:length(y),
#'       (init_state$l + init_state$b*(-11:length(y))) * init_state$s)

#' init_state <- initialize_states(y = y, m = 12, method = "additive")
#' lines(-11:length(y),
#'       init_state$l + init_state$b*(-11:length(y)) + init_state$s,
#'       col = "red")
#'
#' init_state <- initialize_states(y = y, m = 12, method = "additive",
#'                                 init_window_length = 24)
#'
#' lines(-11:length(y),
#'       init_state$l + init_state$b*(-11:length(y)) + init_state$s,
#'       col = "blue")
#'
initialize_states <- function(y,
                              m,
                              method,
                              s = NULL,
                              seasonality_threshold = 0.5,
                              init_window_length = 5) {

  checkmate::assert_numeric(
    x = y, any.missing = TRUE, min.len = 1, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = m, any.missing = FALSE, null.ok = FALSE, len = 1, lower = 1
  )
  checkmate::assert_choice(
    x = method, choices = c("additive", "multiplicative"), null.ok = FALSE
  )
  checkmate::assert_numeric(
    x = seasonality_threshold, lower = 0, upper = 1, len = 1,
    any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = init_window_length, lower = 1, len = 1,
    any.missing = FALSE, null.ok = FALSE
  )

  if (all(is.na(y))) {
    stop("All observations of `y` are NA: No states can be initialized when the series is entirely missing.") # nolint
  }

  x <- y
  n_obs <- length(x)

  # Initialize seasonal component ----

  s_provided_by_user <- FALSE
  if (test_init_state_component(x = s, n_obs = n_obs, m = m)) {
    s_provided_by_user <- TRUE
    s_orig <- s
    s <- s[-(1:m)]
  } else {
    if (!is.null(s)) {
      warning("The provided initial state `s` will be overwritten as it does not fit to the time series and expected period length.") # nolint
    }

    ## Derive global level and trend state using repeated median regression ----
    rm_fit_global <- repeated_median_line(x = x)

    s <- initialize_season(
      x = x,
      fitted = rm_fit_global$fitted,
      m = m,
      threshold = seasonality_threshold,
      method = method
    )
  }

  # Remove seasonal component, (re-)initialize global trend and season ----

  remainder_global <- get_remainder_component(
    x = x, component = s, method = method
  )

  rm_fit_global <- repeated_median_line(x = remainder_global)

  fitted_global <- get_fitted(
    level = rm_fit_global$fitted, seasonal = s, method = method
  )

  residuals_global <- get_residuals(x = x, fitted = fitted_global)
  anomaly_candidates <- abs(residuals_global) >
    # use `pmax()` to prevent flagging nearly-zero valued errors as anomalies
    # when all residuals are essentially zero
    pmax(3 * stats::mad(x = residuals_global, na.rm = TRUE), 1e-10)

  # Remove seasonal component, re-initialize trend and level locally ----

  init_window_length <- pmin(init_window_length, n_obs)
  x_idx <- seq_len(init_window_length)

  remainder_local <- get_remainder_component(
    x = x[x_idx], component = s[x_idx], method = method
  )

  rm_fit_local <- repeated_median_line(x = remainder_local)

  fitted_local <- get_fitted(
    level = rm_fit_local$fitted, seasonal = s[x_idx], method = method
  )

  # Extend states length of time series plus m, return ----

  l <- rep(rm_fit_local$intercept, n_obs + m)
  b <- rep(rm_fit_local$slope, n_obs + m)

  if (s_provided_by_user) {
    s <- s_orig
  } else if (length(s) == 1L) {
    s <- rep(s, length.out = n_obs + m)
  } else if (m > n_obs) {
    # `s` will be the default season, repeated `n_obs` times
    s <- c(rep(s[1], length.out = m), s)
  } else if (length(s) == n_obs) {
    s <- rep(s[1:m], length.out = n_obs + m)
  } else {
    stop(paste0(
      "Before returning, `s` has an unexpected length of ", length(s), "."
    ))
  }

  if (anyNA(s)) {
    stop("The created seasonal component `s` contains missing values. Please double check.") # nolint
  }

  return(
    list(
      l = l,
      b = b,
      s = s,
      l_global = rm_fit_global$intercept,
      b_global = rm_fit_global$slope,
      fitted_local = fitted_local,
      fitted_global = fitted_global,
      anomaly_candidates = anomaly_candidates,
      residuals_global = residuals_global
    )
  )
}

#' Fit a level and trend to a time series using Repeated Median Regression
#'
#' @param x Numeric vector representing a time series
#'
#' @return A list of `intercept`, `slope`, `fitted` values, and `residuals`
#'
#' @references https://en.wikipedia.org/wiki/Repeated_median_regression
#' @keywords internal
#' @examples
#'
#' y <- 1:100 + rnorm(100)
#' tulip:::repeated_median_line(y)
#'
#' y <- 1:28 + rt(n = 28, df = 1)
#' y_idx <- 1:length(y)
#' lm_fit <- lm(y ~ 1 + y_idx)
#' rm_fit <- tulip:::repeated_median_line(y)
#' plot(y, pch = 19, cex = 0.5)
#' abline(a = 0, b = 1, lty = 2) # ideal fit
#' abline(
#'   a = lm_fit$coefficients[1],
#'   b = lm_fit$coefficients[2],
#'   col = "darkblue"
#' )
#' abline(
#'   a = rm_fit$intercept,
#'   b = rm_fit$slope,
#'   col = "darkorange"
#' )
#'
#' y <- as.numeric(RobStatTM::resex)
#' y_idx <- 1:length(y)
#' lm_fit <- lm(y ~ 1 + y_idx)
#' rm_fit <- tulip:::repeated_median_line(y)
#' plot(y, pch = 19, cex = 0.5)
#' abline(
#'   a = lm_fit$coefficients[1],
#'   b = lm_fit$coefficients[2],
#'   col = "darkblue"
#' )
#' abline(a = rm_fit$intercept, b = rm_fit$slope, col = "darkorange")
#'
repeated_median_line <- function(x) {
  checkmate::assert_numeric(x = x, null.ok = FALSE, min.len = 1)

  n_obs <- length(x)

  if (n_obs == 1) {
    return(
      list(
        intercept = x,
        slope = 0,
        fitted = x,
        residuals = x - x # returns NA when x is NA, else 0
      )
    )
  }

  diffs <- rep(NA, n_obs)
  x_idx <- 1:n_obs
  for (j in 1:n_obs) {
    diffs[j] <- stats::median(
      (x[-j] - x[j]) / (x_idx[-j] - x_idx[j]), na.rm = TRUE
    )
  }
  slope <- stats::median(diffs, na.rm = TRUE)

  # Derive initial level state as y-intercept at time 0 ----
  intercept <- stats::median(x - slope * x_idx, na.rm = TRUE)

  fitted <- intercept + slope * x_idx
  residuals <- x - fitted

  return(
    list(
      intercept = intercept,
      slope = slope,
      fitted = fitted,
      residuals = residuals
    )
  )
}

#' Initialize the seasonal state given actuals and previous fitted
#'
#' @param x Numeric vector of actuals
#' @param fitted Numeric vector of fitted values of a level and trend component
#' @param m The period length of the seasonal component
#' @param method Are we using an `additive` or a `multiplicative` seasonal
#'     component?
#' @param threshold A value between 0 and 1; return a neutral default seasonal
#'     component when the seasonal component's reduction of variance in the
#'     residuals is less than the threshold
#'
#' @keywords internal
#'
#' @examples
#' x <- rt(n = 55, df = 3) + 5 * cospi((1:55) / 6)
#' x[seq_along(x) %% 12 == 0] <- NA_real_
#'
#' plot(x, pch = 19)
#' lines(x)
#'
#' fitted <- rep(median(x, na.rm = TRUE), length(x))
#'
#' s <- tulip:::initialize_season(
#'   x = x,
#'   fitted = fitted,
#'   m = 12,
#'   method = "multiplicative",
#'   threshold = 0.5
#' )
#'
#' lines(s * fitted, col = "blue", lty = 2)
#'
initialize_season <- function(x, fitted, m, method, threshold) {
  checkmate::assert_numeric(x = x, min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(x = fitted, len = length(x), null.ok = FALSE)
  checkmate::assert_integerish(x = m, lower = 1, any.missing = FALSE,
                               null.ok = FALSE)
  checkmate::assert_choice(x = method, null.ok = FALSE,
                           choices = c("additive", "multiplicative"))
  checkmate::assert_numeric(x = threshold, lower = 0, upper = 1, len = 1,
                            any.missing = FALSE, null.ok = FALSE)

  n_obs <- length(x)
  if (n_obs == 0) {
    return(numeric())
  }

  s_default <- make_default_season_state(n = n_obs, method = method)

  if (length(x) <= (2 * m + 1) || m == 1) {
    # return early as the input is too short or as no seasonality is expected
    return(s_default)
  }

  remainder <- get_remainder_component(
    x = x,
    component = fitted,
    method = method
  )

  # Derive initial seasonal state estimate using repeated medians ----

  s <- estimate_season(x = remainder, m = m, na_fill = s_default[1])

  fitted_after_seasonality <- get_fitted(
    level = fitted,
    seasonal = s,
    method = method
  )

  residuals <- get_residuals(x = x, fitted = fitted)

  residuals_after_seasonality <- get_residuals(
    x = x,
    fitted = fitted_after_seasonality
  )

  seasonal_component_is_large <- has_large_seasonal_component(
    residuals = residuals,
    residuals_after_seasonality = residuals_after_seasonality,
    threshold = threshold
  )

  if (!seasonal_component_is_large) {
    return(s_default)
  }

  # Sum of initial seasonal states should sum to 0 ----
  if (method == "additive") {
    s <- s - mean(s[1:m])
  }

  return(s)
}

#' Compute the fitted values given `level` and `seasonal` component,
#' depending on whether we use `additive` or `multiplicative` components
#'
#' @param level A numeric vector representing the level (and trend) component
#' @param seasonal A numeric vector representing the seasonal component
#' @param method Are we using `additive` or `multiplicative` components?
#'
#' @return A numeric vector of the same length as `level` and `seasonal`
#'
#' @keywords internal
#'
get_fitted <- function(level, seasonal, method) {
  checkmate::assert_numeric(x = level, min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(x = seasonal, len = length(level), null.ok = FALSE)
  checkmate::assert_choice(x = method, null.ok = FALSE,
                           choices = c("additive", "multiplicative"))

  if (method == "additive") {
    fitted <- level + seasonal
  } else if (method == "multiplicative") {
    fitted <- level * seasonal
  }

  return(fitted)
}

#' Compute the remaining component in `x` given that `component` has been
#' accounted for
#'
#' @param x A numeric vector representing the actuals
#' @param component A numeric vector representing one of two components of `x`
#' @param method Are we using `additive` or `multiplicative` components?
#'
#' @return A numeric vector of the same length as `x` and `component`
#'
#' @keywords internal
#'
get_remainder_component <- function(x, component, method) {
  checkmate::assert_numeric(x = x, min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(x = component, len = length(x), null.ok = FALSE)
  checkmate::assert_choice(x = method, null.ok = FALSE,
                           choices = c("additive", "multiplicative"))

  if (method == "additive") {
    remainder <- x - component
  } else if (method == "multiplicative") {
    remainder <- x / component
    remainder <- ifelse(is.nan(remainder) | is.infinite(remainder),
                        NA_real_,
                        remainder)
  }

  return(remainder)
}

#' Compute residuals from actuals `x` and fitted values `fitted`
#'
#' @param x A numeric vector representing the actuals
#' @param fitted A numeric vector representing the fitted values
#'
#' @return A numeric vector of the same length as `x` and `fitted`
#'
#' @keywords internal
#'
get_residuals <- function(x, fitted) {
  checkmate::assert_numeric(x = x, min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(x = fitted, len = length(x), null.ok = FALSE)

  residuals <- x - fitted

  return(residuals)
}

#' Make a default seasonal state vector
#'
#' Returns a neutral seasonal state vector. Neutral means a vector of 0s for an
#' additive seasonal component, or a vector of 1s for a multiplicative seasonal
#' component.
#'
#' @param n The target length of the state vector
#' @param method Are we using an `additive` or a `multiplicative` seasonal
#'     state component?
#'
#' @return A numeric vector of length `n`; all zero if additive seasonal
#'     component, all one if multiplicative seasonal component
#'
#' @keywords internal
#'
#' @examples
#' tulip:::make_default_season_state(n = 14, method = "additive")
#' tulip:::make_default_season_state(n = 63, method = "multiplicative")
#'
make_default_season_state <- function(n, method) {
  checkmate::assert_integerish(x = n, lower = 1, any.missing = FALSE,
                               null.ok = FALSE)
  checkmate::assert_choice(x = method, null.ok = FALSE,
                           choices = c("additive", "multiplicative"))

  if (method == "additive") {
    s <- rep(0, length.out = n)
  } else if (method == "multiplicative") {
    s <- rep(1, length.out = n)
  } else {
    stop("`method` must be one of: 'additive', 'multiplicative'")
  }

  return(s)
}

#' Get the median value for each seasonal period
#'
#' @param x Values to be explained by seasonal effects
#' @param m The seasonal length
#' @param na_fill Value used to replace NAs (usually 1 or 0 or NA)
#'
#' @return Numeric vector of length equal to that of `x`
#'
#' @keywords internal
#'
#' @examples
#' x <- rt(n = 55, df = 3) + 5 * cospi((1:55) / 6)
#' x[seq_along(x) %% 12 == 0] <- NA_real_
#'
#' plot(x, pch = 19)
#' lines(x)
#'
#' s <- tulip:::estimate_season(x = x, m = 12, na_fill = 0)
#' lines(s, col = "blue", lty = 2)
#'
#' tulip:::estimate_season(x = x[1:7], m = 12, na_fill = 0)
#' lines(s, col = "blue", lty = 2)
#'
estimate_season <- function(x, m, na_fill) {
  checkmate::assert_numeric(x = x, min.len = 1, null.ok = FALSE)
  checkmate::assert_integerish(x = m, lower = 1, any.missing = FALSE,
                               null.ok = FALSE)
  checkmate::assert_choice(x = na_fill, choices = c(0, 1, NA_real_),
                           null.ok = FALSE)

  n_obs <- length(x)

  s_1_to_m <- apply(
    # need to fill up y to full season
    X = matrix(c(x, rep(NA, ceiling(n_obs / m) * m - n_obs)),
               ncol = m, byrow = TRUE),
    FUN = stats::median,
    MARGIN = 2,
    na.rm = TRUE
  )

  # NA values could occur when all values for one of the periods are NA
  s_1_to_m <- ifelse(is.na(s_1_to_m), na_fill, s_1_to_m)
  s <- rep_len(s_1_to_m, length.out = n_obs)

  return(s)
}

#' Compare residuals to judge whether series has a sizeable seasonal component
#'
#' @param residuals Residuals from prediction without seasonal component
#' @param residuals_after_seasonality Residuals from prediction with seasonal
#'     component
#' @param threshold A value between 0 and 1 describing the share of variation
#'     removed from the residuals due to inclusion of the seasonal component
#'
#' @return Logical scalar
#' @keywords internal
#'
#' @examples
#' residuals_after_seasonality <- rnorm(n = 50)
#' residuals <- residuals_after_seasonality + 5 * cospi((1:50) / 6)
#'
#' plot(residuals, pch = 19)
#' lines(residuals, lty = 1)
#' points(residuals_after_seasonality)
#' lines(residuals_after_seasonality, lty = 2)
#'
#' tulip:::has_large_seasonal_component(
#'   residuals = residuals,
#'   residuals_after_seasonality = residuals_after_seasonality,
#'   threshold = 0.5
#' )
#'
has_large_seasonal_component <- function(residuals,
                                         residuals_after_seasonality,
                                         threshold) {

  checkmate::assert_numeric(x = residuals, null.ok = FALSE)
  checkmate::assert_numeric(x = residuals_after_seasonality, null.ok = FALSE,
                            len = length(residuals))
  checkmate::assert_numeric(x = threshold, lower = 0, upper = 1, len = 1,
                            any.missing = FALSE, null.ok = FALSE)

  # set to zero if seasonal component is small compared to residual error;
  # use `mad()` as robust alternative for `sd()`; the seasonality is supposed
  # to explain at least 100*`seasonality_threshold`% of the residual variance
  seasonality_size <- 1 -
    stats::mad(residuals_after_seasonality, na.rm = TRUE)^2 /
    stats::mad(residuals, na.rm = TRUE)^2

  answer <- isTRUE(seasonality_size > threshold)
  return(answer)
}

test_init_state_component <- function(x, n_obs, m) {
  checkmate::test_numeric(
    x = x, finite = TRUE, any.missing = FALSE, len = n_obs + m, null.ok = FALSE
  )
}

dispatch_initialize_states <- function(init_states,
                                       y,
                                       m,
                                       method,
                                       seasonality_threshold) {

  n_obs <- length(y)

  l_and_b_are_provided <- test_init_state_component(
    x = init_states$l, n_obs = n_obs, m = m
  ) && test_init_state_component(
    x = init_states$b, n_obs = n_obs, m = m
  )

  s_is_provided <- test_init_state_component(
    x = init_states$s, n_obs = n_obs, m = m
  )

  all_states_are_provided <- !is.null(init_states) &&
    l_and_b_are_provided &&
    s_is_provided

  if (all_states_are_provided) {
    return(init_states)
  }

  if (l_and_b_are_provided && !s_is_provided) {
    warning("The provided `init_states$l` and `init_states$b` will be overwritten as no suitable `init_states$s` is available. See also `?initialize_states().") # nolint
  } else if (!is.null(init_states) && !s_is_provided) {
    warning("A user-provided `init_states` object must contain at least state component `s`, optionally also both `l` and `b`. The provided `init_states` object will be overwritten as the required components are not available. See also `?initalize_states()`.") # nolint
  }

  init_states <- initialize_states(
    y = y,
    m = m,
    method = method,
    seasonality_threshold = seasonality_threshold,
    s = init_states$s
  )

  return(init_states)
}

#' Use a previous fit's initialization as initialization for a new time series
#'
#' This function assumes that the provided `y` is a continuation of the time
#' series used to derive the provided `object`. The first `length(object$y)`
#' observations should be equal across `y` and `object$y`.
#'
#' @param object The fitted model object returned by [tulip::tulip()] of class
#'     `tulip`
#' @param y A time series as numeric vector, may include NAs for some (but not
#'   all) of the observations
#' @param use_new_residuals_for_anomaly_candidates Logical (default `FALSE`); if
#'   `TRUE`, the residuals of the new time series will be used to judge whether
#'   some observations are anomaly candidates; else only the first
#'   `length(object$y)` observations will be used
#'
#' @keywords internal
initialize_states_using_previous_fit <- function(object,
                                                 y,
                                                 use_new_residuals_for_anomaly_candidates = FALSE) {

  n_previous <- length(object$y)

  l <- rep(object$l_init[1], length.out = object$m + length(y))
  b <- rep(object$b_init[1], length.out = object$m + length(y))
  s <- rep(object$s_init, length.out = object$m + length(y))

  intercept_global <- object$init_states$l_global
  slope_global <- object$init_states$b_global

  y_idx <- seq_along(y)
  level_global <- intercept_global + slope_global * y_idx

  fitted_global <- get_fitted(
    level = level_global,
    seasonal = s[-(seq_len(object$m))],
    method = object$method
  )

  residuals_global <- get_residuals(x = y, fitted = fitted_global)

  if (use_new_residuals_for_anomaly_candidates) {
    residuals_mad <- stats::mad(x = residuals_global, na.rm = TRUE)
  } else {
    residuals_mad <- stats::mad(x = residuals_global[seq_len(n_previous)],
                                na.rm = TRUE)
  }

  anomaly_candidates <- abs(residuals_global) >
    # use `pmax()` to prevent flagging nearly-zero valued errors as anomalies
    # when all residuals are essentially zero
    pmax(3 * residuals_mad, 1e-10)

  return(
    list(
      l = l,
      b = b,
      s = s,
      l_global = intercept_global,
      b_global = slope_global,
      anomaly_candidates = anomaly_candidates,
      residuals_global = residuals_global
    )
  )
}
