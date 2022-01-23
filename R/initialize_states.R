#' Initialize state components given time series
#'
#' @export
initialize_states <- function(y, m) {

  # Examples to try against:
  # y <- rnorm(n = 60, mean = 1:60)

  # See initialization described in Crevits R, Croux C., "Forecasting using
  # robust exponential smoothing with damped trend and seasonal components"
  # https://core.ac.uk/download/pdf/84933237.pdf

  # Pad x with NAs that will be used to the length of the
  # vectors that hold the state components including the
  # initial states
  x <- y
  y <- c(rep(NA, m), y)
  n_obs <- length(y)

  # the level is initialized as the median of x
  l <- rep(median(y, na.rm = TRUE), n_obs)

  # the trend is initialized as median of the median
  # period-over-period changes

  # for each observation, get the standardized change compared to
  # every other observations
  med_i_not_j <- rep(NA, n_obs)
  for (i in 1:n_obs) {
    med_i_not_j[i] <- median((y[i] - y[-i]) / (i - (1:n_obs)[-i]), na.rm = TRUE)
  }

  # the median of those changes is the initial state of the trend
  b <- rep(median(med_i_not_j, na.rm = TRUE), n_obs)
  # b <- rep(0, n_obs)

  if (length(x) %% 2 == 0) {
    trend_idx <- (-length(x)/2 + 1):(length(x)/2)
  } else {
    trend_idx <- (-(length(x)-1)/2):((length(x)-1)/2)
  }

  # remove level and trend before trying to identify the seasonality
  # to not mix the other signals into the seasonality
  x_remainder <- x - (unique(l) + unique(b) * trend_idx)

  # TODO: l needs to be adjusted to the first obs

  # TODO: See also approach from `forecast::tsoutliers()` to detect seasonality
  #       https://github.com/robjhyndman/forecast/blob/master/R/clean.R

  s_1_to_m <- apply(
    # need to fill up y to full season
    X = matrix(c(x_remainder, rep(NA, ceiling(n_obs / m) * m - n_obs)),
               ncol = m, byrow = TRUE),
    FUN = median,
    MARGIN = 2,
    na.rm = TRUE
  )

  s <- rep_len(s_1_to_m, length.out = n_obs)

  return(
    list(
      l = l,
      b = b,
      s = s
    )
  )
}
