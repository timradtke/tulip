#' Initialize state components given time series
#'
#' @param y Input time series to fit the model against
#' @param m Scalar indicating the period length of the potential seasonality
#' @param shift_detection Logical; should the automatic shift detection be run?
#' @param window_size Integer defining the size of the windows used to detect
#'     structural shifts in the time series; since we compare windows based on
#'     the median of the trends in the window, odd values are easier to handle.
#'     `window_size` limits how precise the trend shift can be identified. The
#'     shift cannot be identified more precise than within a window of
#'     `window_size` candidate values.
#' @param seasonality_threshold Share of residual variance to be explained by
#'     seasonal component at least if it is to be used
#'
#' @export
initialize_states <- function(y,
                              m,
                              shift_detection = FALSE,
                              window_size = 5,
                              seasonality_threshold = 0.5,
                              verbose = TRUE) {

  # See initialization described in Crevits R, Croux C., "Forecasting using
  # robust exponential smoothing with damped trend and seasonal components"
  # https://core.ac.uk/download/pdf/84933237.pdf

  if (all(is.na(y))) {
    stop("All observations of `y` are NA: No states can be initialized when the series is entirely missing.") # no lint
  }

  x <- y
  y <- c(rep(NA, m), y)
  n_obs <- length(y)

  # Instead of initializing by the first observation, we try to make it a bit
  # more robust against outliers on observation 1 by using the median over the
  # first three values; this should be okay-ish even if there are a trend and
  # seasonal component
  x_not_na_idx <- which(!is.na(x))
  # construct the level from the first (up-to) three non-NA observations
  l <- rep(median(x[x_not_na_idx[1:min(3, length(x_not_na_idx))]]) -
             median(x, na.rm = TRUE), n_obs)

  # The trend is initialized as median of the median period-over-period changes;
  # for each i, get the standardized change compared to all other observations.
  # Then, the median of these changes is used as initial guess for the trend.
  med_i_not_j <- rep(NA, n_obs)
  for (i in 1:n_obs) {
    med_i_not_j[i] <- median((y[i] - y[-i]) / (i - (1:n_obs)[-i]), na.rm = TRUE)
  }
  b <- rep(median(med_i_not_j, na.rm = TRUE), n_obs)
  if (anyNA(b)) b <- rep(b, n_obs)

  shift_range <- c(NA, NA)
  if (shift_detection && abs(unique(b)) > (1 / length(x))) {
    shift_range <- detect_shift(
      trends = na.omit(med_i_not_j),
      window_size = window_size,
      verbose = verbose
    )
  }

  # remove level and trend before trying to identify the seasonality to not mix
  # other signals into the seasonality
  trend_idx <- 0:(length(x)-1)
  x_remainder <- x - (unique(l) + unique(b) * trend_idx)

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
      seasonality_size <- 1 - mad(x_remainder - s[-(1:m)], na.rm = TRUE)^2 /
        mad(x_remainder, na.rm = TRUE)^2
      if (isTRUE(seasonality_size < seasonality_threshold)) {
        s <- rep(0, length.out = n_obs)
      }
    }
  }

  return(
    list(
      l = l,
      b = b,
      s = s,
      shift_range = shift_range
    )
  )
}
