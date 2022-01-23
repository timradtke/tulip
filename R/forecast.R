#' Draw sample paths from a fitted object to forecast
#'
#' @export
forecast <- function(object, h = 12, n = 10, family = NULL) {

  checkmate::assert_list(
    x = object, any.missing = FALSE, null.ok = TRUE
  )
  checkmate::assert_names(
    x = names(object),
    must.include = c("l", "b", "s", "l_init", "b_init", "s_init",
                     "param_grid", "sigma", "y", "y_hat", "x", "x_hat",
                     "family", "m", "y_median", "y_mad")
  )
  checkmate::assert_integerish(
    x = h, any.missing = FALSE, null.ok = FALSE, len = 1
  )
  checkmate::assert_integerish(
    x = n, any.missing = FALSE, null.ok = FALSE, len = 1
  )
  checkmate::assert_choice(
    x = family, choices = c("norm", "cauchy", "nbinom"), null.ok = TRUE
  )

  if (is.null(family)) family <- object$family

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

  for (i in (m+1):(m+h)) {
    if (family == "nbinom") {
      tmp_y <- rnbinom(
        n = n,
        size = object$sigma,
        mu = expm1(l[i-1, ] + b[i-1, ] + s[i-m, ])
      )
      y[i, ] <- log1p(tmp_y)
    } else {
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
  y <- y[-(1:m), ]

  if (family %in% c("norm", "cauchy")) {
    y <- y * object$y_mad + object$y_median
  } else if (family == "nbinom") {
    y <- expm1(y)
  }

  return(y)
}
