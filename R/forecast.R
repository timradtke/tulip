#' Forecast by drawing sample paths from a fitted object
#'
#' Instead of generating a point forecast and marginal forecast intervals based
#' on a Normal assumption and closed-from equations, we draw observations from
#' the observation equation. The uncertainty is then represented via samples
#' from the forecast distribution. This allows us to represent the distribution
#' over multiple horizons instead of only the marginal distribution at
#' individual horizons.
#'
#' The returned value is a matrix of dimensions `h` times `n`. Each of the `n`
#' columns represents one sample path and is a random draw from the
#' `h`-dimensional forecast distribution. See also examples below.
#'
#' @param object The fitted model object returned by [fit()].
#' @param h The forecast horizon as integer number of periods
#' @param n The integer number of sample paths to draw from the forecast
#'     distribution
#' @param switch_to_cauchy_if_outliers If `TRUE`, then if the fitted model uses
#'     a Normal distribution as likelihood but outliers were detected and
#'     interpolated during fitting of the smoothing parameters, then the
#'     forecast distribution can automatically switch to a Cauchy distribution
#'     from which sample paths are drawn instead. Default is `FALSE`.
#'
#' @seealso [fit()]
#' @export
#'
#' @examples
#' set.seed(4278)
#'
#' y <- rt(100, df = 10) * 10 + 1:100
#' # plot(y, type = "l", col = "grey", xlab = NA)
#' # points(y, pch = 21, bg = "black", col = "white")
#'
#' ls_fit <- fit(y = y, m = 12, family = "norm")
#'
#' m_fc <- forecast(object = ls_fit, h = 12, n = 10000)
#'
#' # summarize over draws (columns) to get point forecasts
#' rowMeans(m_fc)
#'
#' # marginal quantiles can be derived in much the same way
#' apply(m_fc, 1, quantile, 0.05)
#' apply(m_fc, 1, quantile, 0.95)
#'
#' # we can also summarize the distribution of the sum over horizons 4 to 6
#' quantile(colSums(m_fc[4:6, ]), c(0.05, 0.5, 0.95))
#'
#' # sample paths carry autocorrelation, the horizons are not independent of
#' # another; this is revealed when we drop the autocorrelation by shuffling
#' # at each margin randomly and try to recompute the same quantiles as above:
#'
#' m_fc_shuffled <- rbind(
#'   m_fc[4, sample(x = 1:10000, size = 10000, replace = TRUE), drop = FALSE],
#'   m_fc[5, sample(x = 1:10000, size = 10000, replace = TRUE), drop = FALSE],
#'   m_fc[6, sample(x = 1:10000, size = 10000, replace = TRUE), drop = FALSE]
#' )
#' quantile(colSums(m_fc_shuffled), c(0.05, 0.5, 0.95))
#'
#' # one can also look  directly at the correlation
#' cor(m_fc[4, ], m_fc[6, ])
#' cor(m_fc_shuffled[1, ], m_fc_shuffled[3, ])
#'
forecast <- function(object,
                     h = 12,
                     n = 10000,
                     switch_to_cauchy_if_outliers = FALSE) {

  checkmate::assert_list(
    x = object, null.ok = TRUE
  )
  checkmate::assert_names(
    x = names(object),
    must.include = c("l", "b", "s", "l_init", "b_init", "s_init",
                     "param_grid", "sigma", "y", "y_hat", "x", "x_hat",
                     "x_na", "family", "m", "y_median", "y_mad", "comment")
  )
  checkmate::assert_integerish(
    x = h, any.missing = FALSE, null.ok = FALSE, len = 1
  )
  checkmate::assert_integerish(
    x = n, any.missing = FALSE, null.ok = FALSE, len = 1
  )

  if (isTRUE(object$comment == "no_variance")) {
    return(
      matrix(unique(object$y), ncol = n, nrow = h)
    )
  }
  if (isTRUE(object$comment == "single_obs")) {
    return(
      matrix(unique(object$y), ncol = n, nrow = h)
    )
  }
  if (isTRUE(object$comment == "mad_zero")) {
    warning("Using a bootstrap forecast since the MAD of the input `y` is 0.")
    return(
      matrix(sample(x = object$y, size = n * h, replace = TRUE),
             ncol = n, nrow = h)
    )
  }

  family <- object$family

  if (family == "norm" &&
      switch_to_cauchy_if_outliers &&
      object$n_cleaned > 1 + ceiling(0.01 * length(object$x))) {
    family <- "cauchy"
    object$sigma <- IQR(x = object$x_hat - object$x) / 2
  }

  # draw IID errors to be used when creating sample paths
  if (family == "cauchy") {
    m_e <- matrix(
      rcauchy(n = h * n, location = 0, scale = object$sigma),
      ncol = n
    )
  } else if (family == "norm") {
    m_e <- matrix(
      rnorm(n = h * n, mean = 0, sd = object$sigma),
      ncol = n
    )
  } else if (family == "student") {
    m_e <- matrix(
      rt(n = h * n, df = 5) * object$sigma,
      ncol = n
    )
  }

  m <- object$m

  checkmate::assert_numeric(
    x = object$param_grid, min.len = 6, max.len = 6, upper = 1, lower = 0,
    null.ok = FALSE, any.missing = FALSE
  )
  checkmate::assert_names(
    x = names(object$param_grid),
    identical.to = c("alpha", "one_minus_alpha",
                     "beta", "one_minus_beta",
                     "gamma", "one_minus_gamma")
  )

  alpha <- object$param_grid[["alpha"]]
  beta <- object$param_grid[["beta"]]
  gamma <- object$param_grid[["gamma"]]

  one_minus_alpha <- object$param_grid[["one_minus_alpha"]]
  one_minus_beta <- object$param_grid[["one_minus_beta"]]
  one_minus_gamma <- object$param_grid[["one_minus_gamma"]]

  # select the latest state from which we continue into the future
  l <- object$l[length(object$l)]
  b <- object$b[length(object$b)]
  # we need one full period of seasonal state even if series was shorter
  s_max_len <- length(c(object$s_init, object$s))
  s <- c(object$s_init, object$s)[(s_max_len - m + 1):s_max_len]

  # expand for (seasonal + forecast) x (sample paths)
  l <- matrix(rep(l, (m + h) * n), ncol = n)
  b <- matrix(rep(b, (m + h) * n), ncol = n)
  s <- matrix(rep_len(s, length.out = (m + h) * n), ncol = n, byrow = FALSE)

  y <- matrix(rep(NA, (m + h) * n), ncol = n)
  y_orig <- matrix(rep(NA, (m + h) * n), ncol = n)
  m_e_orig <- m_e
  fit_clean_sigma <- sd(object$x_na, na.rm = TRUE)

  for (i in (m+1):(m+h)) {
    if (family == "nbinom") {
      tmp_y <- rnbinom(
        n = n,
        size = object$sigma,
        mu = expm1(l[i-1, ] + b[i-1, ] + s[i-m, ])
      )
      y[i, ] <- log1p(tmp_y)
    } else {

      y_orig[i, ] <- l[i-1, ] + b[i-1, ] + s[i-m, ] + m_e[i-m, ]

      if (!is.na(fit_clean_sigma) && family == "cauchy") {
        is_outlier <- pnorm(
          q = abs(m_e[i-m, ]),
          mean = 0,
          sd = fit_clean_sigma
        ) > 0.99

        # use `y_hat` instead of `y` to continue update of parameters
        m_e[i-m, ] <- ifelse(is_outlier, 0, m_e[i-m, ])
      }

      y[i, ] <- l[i-1, ] + b[i-1, ] + s[i-m, ] + m_e[i-m, ]
    }

    l[i, ] <- alpha * (y[i, ] - s[i-m, ]) +
      one_minus_alpha * (l[i-1, ] + b[i-1, ])
    b[i, ] <- beta * (l[i, ] - l[i-1, ]) +
      one_minus_beta * b[i-1, ]
    s[i, ] <- gamma * (y[i, ] - l[i-1, ] - b[i-1, ]) +
      one_minus_gamma * s[i-m, ]
  }

  # drop the placeholders used for initial states
  y_orig <- y_orig[-(1:m), ]

  if (family %in% c("norm", "cauchy", "student")) {
    y_orig <- y_orig * object$y_mad + object$y_median
  } else if (family == "nbinom") {
    y_orig <- expm1(y_orig)
  }

  return(y_orig)
}
