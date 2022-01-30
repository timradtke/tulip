#' Detect shifts from sign to opposite sign in series
#'
#' @param trends Series of period-over-period differences describing the trend;
#'     as created in `initialize_states()` to compute the initial trend state.
#' @param window_size Integer defining the size of the windows used to detect
#'     structural shifts in the time series; since we compare windows based on
#'     the median of the trends in the window, odd values are easier to handle.
#'     `window_size` limits how precise the trend shift can be identified. The
#'     shift cannot be identified more precise than within a window of
#'     `window_size` candidate values.
#' @param verbose Logical; should a warning summarizing a detected shift be
#'     printed?
#'
detect_shift <- function(trends, window_size, verbose = TRUE) {

  shift_range <- c(NA, NA)
  n <- length(trends)

  window_size <- ifelse(window_size >= 5, window_size, 5)
  if (length(trends) < 2*window_size) {
    return(shift_range)
  }

  indices_larger_0 <- which(trends > 0)
  indices_smaller_0 <- which(trends <= 0)
  shift_detected <- FALSE

  if (length(indices_larger_0) > 0 && length(indices_smaller_0) > 0) {
    if (trends[1] > 0) {
      if (max(indices_larger_0) < min(indices_smaller_0)) {
        shift_detected <- TRUE
        shift_range <- c(max(indices_larger_0), max(indices_larger_0) + 1)
      }
    } else if (trends[1] < 0) {
      if (max(indices_smaller_0) < min(indices_larger_0)) {
        shift_detected <- TRUE
        shift_range <- c(max(indices_smaller_0), max(indices_smaller_0) + 1)
      }
    }
  }

  if (!shift_detected) {
    trend_windows <- apply(
      # only use the windows for which we have window_size observations
      X = matrix(trends[-(1:(n %% window_size))],
                 ncol = window_size, byrow = TRUE),
      FUN = median,
      MARGIN = 1,
      na.rm = TRUE
    )

    if (trend_windows[length(trend_windows)] > 0) {
      # after this, the last trend window should have a negative sign
      trend_windows <- -1 * trend_windows
    }
    if (any(trend_windows >= 0)) { # if any window has opposite sign of final w
      window_opposite_trend <- max(which(trend_windows >= 0))
      shift_range <- c(
        ceiling(window_opposite_trend * window_size - 0.5 * window_size),
        floor((window_opposite_trend + 1) * window_size - 0.5 * window_size)
      )
      shift_range <- n %% window_size + shift_range
    }
  }

  if (!anyNA(shift_range) && verbose) {
    warning(
      sprintf(
        "Potential (final) structural shift between observations [%s, %s] detected.
Consider re-running using only observations (%s, ...] to avoid unreasonably large `beta` estimate.
Or is the time series currently inactive?",
shift_range[1], shift_range[2], shift_range[2] + 1
      )
    )
  }

  return(shift_range)
}
