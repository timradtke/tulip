#' Initialize state components given time series
#'
#' @param y Input time series to fit the model against
#' @param m Scalar indicating the period length of the potential seasonality
#' @param seasonality_threshold Share of residual variance to be explained by
#'     seasonal component at least if it is to be used
#'
#' @export
#' @examples
#' y <- as.numeric(AirPassengers[1:68])
#' init_state <- initialize_states(y = y, m = 12, seasonality_threshold = 0.5)
#'
#' plot(-11:length(y), c(rep(NA, 12), y), pch = 19, cex = 0.5)
#' lines(-11:length(y),
#'       init_state$l + init_state$b*(-11:length(y)) + init_state$s)
#'
#' y <- as.numeric(RobStatTM::resex)
#' init_state <- initialize_states(y = y, m = 12, seasonality_threshold = 0.5)
#'
#' plot(-11:length(y), c(rep(NA, 12), y), pch = 19, cex = 0.5)
#' lines(-11:length(y),
#'       init_state$l + init_state$b*(-11:length(y)) + init_state$s)
#'
#' init_state <- initialize_states(y = y, m = 12, init_window_length = 24)
#'
#' plot(-11:length(y), c(rep(NA, 12), y), pch = 19, cex = 0.5)
#' lines(-11:length(y),
#'       init_state$l + init_state$b*(-11:length(y)) + init_state$s)
#'
initialize_states <- function(y,
                              m,
                              seasonality_threshold = 0.5,
                              init_window_length = 5) {

  # See initialization described in Crevits R, Croux C., "Forecasting using
  # robust exponential smoothing with damped trend and seasonal components"
  # https://core.ac.uk/download/pdf/84933237.pdf

  if (all(is.na(y))) {
    stop("All observations of `y` are NA: No states can be initialized when the series is entirely missing.") # no lint
  }

  x <- y
  n_obs <- length(x)

  # Derive global level and trend state using repeated median regression ----
  rm_fit_global <- repeated_median_line(x = x)

  s <- initialize_season(
    x = x,
    x_remainder = rm_fit_global$residuals,
    m = m,
    n_obs = n_obs,
    threshold = seasonality_threshold
  )

  # Remove seasonal component, re-initialize global trend and season ----

  rm_fit_global <- repeated_median_line(x = x - s)

  fitted_global <- rm_fit_global$fitted + s
  residuals_global <- x - fitted_global
  anomaly_candidates <- abs(residuals_global) >
    3 * mad(x = residuals_global, na.rm = TRUE)

  # Remove seasonal component, re-initialize trend and level locally ----

  x_idx <- 1:init_window_length
  rm_fit_local <- repeated_median_line(x = x[x_idx] - s[x_idx])

  l <- rep(rm_fit_local$intercept, n_obs + m)
  b <- rep(rm_fit_local$slope, n_obs + m)
  s <- rep(s[1:m], length.out = n_obs + m)

  return(
    list(
      l = l,
      b = b,
      s = s,
      l_global = rm_fit_global$intercept,
      b_global = rm_fit_global$slope,
      fitted_local = rm_fit_local$fitted + s[x_idx],
      fitted_global = fitted_global,
      anomaly_candidates = anomaly_candidates
    )
  )
}


#' @examples
#' y <- as.numeric(RobStatTM::resex)
#' y_idx <- 1:length(y)
#' lm_fit <- lm(y ~ 1 + y_idx)
#' rm_fit <- repeated_median_line(y)
#' plot(y, pch = 19, cex = 0.5)
#' abline(a = lm_fit$coefficients[1], b = lm_fit$coefficients[2], col = "darkblue")
#' abline(a = rm_fit$intercept, b = rm_fit$slope, col = "darkorange")
#'
#' y <- 1:100 + rnorm(100)
#' repeated_median_line(y)
#'
repeated_median_line <- function(x) {
  n_obs <- length(x)

  diffs <- rep(NA, n_obs)
  x_idx <- 1:n_obs
  for (j in 1:n_obs) {
    diffs[j] <- median(
      (x[-j] - x[j]) / (x_idx[-j] - x_idx[j]), na.rm = TRUE
    )
  }
  slope <- median(diffs, na.rm = TRUE)

  # Derive initial level state as y-intercept at time 0 ----
  intercept <- median(x - slope * x_idx, na.rm = TRUE)

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

initialize_season <- function(x, x_remainder, m, n_obs, threshold) {
  # Derive initial seasonal state estimate using repeated medians ----
  s <- rep(0, length.out = n_obs)
  if (length(x) > (2 * m + 1) && m > 1) {
    s_1_to_m <- apply(
      # need to fill up y to full season
      X = matrix(c(x_remainder,
                   rep(NA, ceiling(n_obs / m) * m - n_obs)),
                 ncol = m, byrow = TRUE),
      FUN = median,
      MARGIN = 2,
      na.rm = TRUE
    )

    if (anyNA(s_1_to_m)) {
      # this case occurs, for example, if every April observation of a monthly
      # time series is missing
      warning(sprintf("Cannot estimate seasonal initial state as at least one of the %s periods has no non-NA observation.", m)) # no lint
      s <- rep_len(0, length.out = n_obs)
    } else {
      s <- rep_len(s_1_to_m, length.out = n_obs)

      # set to zero if seasonal component is small compared to residual error;
      # use `mad()` as robust alternative for `sd()`; the seasonality is supposed
      # to explain at least 100*`seasonality_threshold`% of the residual variance
      seasonality_size <- 1 - mad(x_remainder - s, na.rm = TRUE)^2 /
        mad(x_remainder, na.rm = TRUE)^2
      if (isTRUE(seasonality_size < threshold)) {
        s <- rep(0, length.out = n_obs)
      }
    }
  }

  # Sum of initial seasonal states should sum to 0 ----
  s_std <- s - mean(s[1:m])

  return(s_std)
}

