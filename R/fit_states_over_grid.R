#' Fit parameters given time series, initial states, and parameter grid
#'
fit_states_over_grid <- function(y, m, init_states, param_grid) {

  n <- length(y)
  k <- dim(param_grid)[1] # number of parameter combinations to trial

  # pad y with NA start values, then expand by number of parameter options
  y <- matrix(c(rep(NA, m), y), nrow = m + n, ncol = k, byrow = FALSE)
  y_hat <- matrix(NA, nrow = m + n, ncol = k)

  # expand states by number of parameter options
  if (length(init_states$l) != m + n) stop("l wrong length")
  if (length(init_states$b) != m + n) stop("b wrong length")
  if (length(init_states$s) != m + n) stop("s wrong length")

  l <- matrix(init_states$l, nrow = m + n, ncol = k, byrow = FALSE)
  b <- matrix(init_states$b, nrow = m + n, ncol = k, byrow = FALSE)
  s <- matrix(init_states$s, nrow = m + n, ncol = k, byrow = FALSE)

  # for each time step, perform the same transformations for each set of
  # parameters in vectorized form

  for (i in (m + 1):(m + n)) {
    y_hat[i, ] <- l[i-1, ] + b[i-1, ] + s[i-m, ]

    l[i, ] <- param_grid[, "alpha"] * (y[i, ] - s[i-m, ]) +
      param_grid[, "one_minus_alpha"] * (l[i-1, ] + b[i-1, ])

    b[i, ] <- param_grid[, "beta"] * (l[i, ] - l[i-1, ]) +
      param_grid[, "one_minus_beta"] * b[i-1, ]

    s[i, ] <- param_grid[, "gamma"] * (y[i, ] - l[i-1, ] - b[i-1, ]) +
      param_grid[, "one_minus_gamma"] * s[i-m, ]
  }

  return(
    list(
      y_hat = y_hat[-(1:m), ],
      y = y[-(1:m), ],
      e = y_hat[-(1:m), ] - y[-(1:m), ],
      l = l[-(1:m), ],
      b = b[-(1:m), ],
      s = s[-(1:m), ],
      l_init = l[1:m, ],
      b_init = b[1:m, ],
      s_init = s[1:m, ]
    )
  )
}
