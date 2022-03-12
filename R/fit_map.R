#' Fit error / observation equation and compute loss
#'
#' @param param_grid A matrix of possible parameter values used in grid search
#' @param priors A named list of lists of prior parameters
#' @param family Distribution to be used as likelihood function
#' @param m Scalar indicating the suspected seasonality
#' @param y Input time series to fit the model against
#' @param y_hat Fitted values after fitting the states and parameters to y
#' @param n_cleaned Number of values treated as outlier
#'
#' @examples
#' y <- matrix(c(1, 0, 1, -0.05), ncol = 2)
#' y_hat <- matrix(c(0.9, 0.25, 1.05, 0.05), ncol = 2)
#' m <- 12
#' family <- "norm"
#' param_grid <- initialize_param_grid()[6:7,]
#' priors <- list(
#'   error = list(
#'     shape = 3,
#'     rate = 2 * 0.75
#'     # inv_gamma_prior_alpha <- 3
#'     # inv_gamma_prior_beta <- (inv_gamma_prior_alpha - 1) * 0.75
#'   ),
#'   level = list(
#'     alpha = 2,
#'     beta = 6
#'   ),
#'   trend = list(
#'     prob = 0.5,
#'     alpha = 2,
#'     beta = 18
#'   ),
#'   seasonality = list(
#'     prob = 0.5,
#'     alpha = 2,
#'     beta = 6
#'   ),
#'   anomaly = list(
#'     prob = 0.01
#'   )
#' )
#'
fit_map <- function(param_grid,
                    priors,
                    family,
                    m,
                    y,
                    y_hat,
                    n_cleaned) {

  checkmate::assert_matrix(x = y, min.rows = 1, min.cols = 1)
  checkmate::assert_matrix(x = y_hat, nrows = nrow(y), ncols = ncol(y))

  # errors can include NAs if y had NAs in the first place; while those are
  # being interpolated in y_hat, we cannot compute errors at those locations.
  # one could evaluate y_hat for those against a prior in the future.
  n_obs <- dim(y)[1] - sum(is.na(y[,1]))
  errors <- y_hat - y

  l_smooth <- log_prior_smooth(
    priors = priors,
    param_grid = param_grid,
    n_obs = n_obs,
    n_cleaned = n_cleaned
  )

  if (family %in% c("norm", "auto")) {
    sigma_norm <- fit_sigma(
      errors = errors,
      n_obs = n_obs,
      method = "norm",
      shape = priors$error$shape,
      scale = priors$error$scale
    )
    l_sigma_norm <- log_prior_sigma(priors = priors, sigma = sigma_norm)
    l_norm <- log_lik_norm(errors = errors, sigma = sigma_norm)
  }
  if (family %in% c("cauchy", "auto")) {
    sigma_cauchy <- fit_sigma(errors = errors, n_obs = n_obs, method = "cauchy")
    l_sigma_cauchy <- log_prior_sigma(priors = priors, sigma = sigma_cauchy)
    l_cauchy <- log_lik_cauchy(errors = errors, sigma = sigma_cauchy)
  }
  if (family %in% c("student", "auto")) {
    if (family != "auto") {
      sigma_norm <- fit_sigma(
        errors = errors,
        n_obs = n_obs,
        method = "norm",
        shape = priors$error$shape,
        scale = priors$error$scale
      )
    }
    sigma_student <- fit_sigma(errors = errors, n_obs = n_obs, method = "student") # no lint
    l_sigma_student <- log_prior_sigma(priors = priors, sigma = sigma_norm)
    l_student <- log_lik_student(errors = errors, sigma = sigma_student, df = 5)
  }

  log_joint <- l_smooth
  idx_wrap <- length(log_joint)

  if (family == "norm") {
    family_joint <- rep(family, length(log_joint))
    sigma <- sigma_norm
    log_joint <- log_joint + l_sigma_norm + l_norm
  } else if (family == "cauchy") {
    family_joint <- rep(family, length(log_joint))
    sigma <- sigma_cauchy
    log_joint <- log_joint + l_sigma_cauchy + l_cauchy
  } else if (family == "student") {
    family_joint <- rep(family, length(log_joint))
    sigma <- sigma_student
    log_joint <- log_joint + l_sigma_student + l_student
  } else if (family == "auto") {
    family_joint <- rep(c("norm", "cauchy", "student"),
                        each = length(log_joint))
    sigma <- c(sigma_norm, sigma_cauchy, sigma_student)
    log_joint <- log_joint +
      c(l_sigma_norm, l_sigma_cauchy, l_sigma_student) +
      c(l_norm, l_cauchy, l_student)
  }

  return(
    list(
      sigma = sigma,
      log_joint = log_joint,
      family = family_joint,
      idx_wrap = idx_wrap
    )
  )
}

dbernoulli <- function(x, prob, log = FALSE) {
  res <- ifelse(as.integer(x) == 0L, 1 - prob, prob)
  if (log == TRUE) res <- log(res)
  return(res)
}

limit <- function(x) {
  pmin(pmax(x, 0.0001), 0.9999)
}

dinvgamma <- function(x, shape, scale, log = FALSE) {
  # https://en.wikipedia.org/wiki/Inverse-gamma_distribution#Probability_density_function
  checkmate::assert_numeric(x = x)
  checkmate::assert_numeric(x = shape, lower = 0, len = 1)
  checkmate::assert_numeric(x = scale, lower = 0, len = 1)
  checkmate::assert_true(shape > 0)
  checkmate::assert_true(scale > 0)

  y <- ifelse(
    x > 0,
    scale^shape / gamma(x = shape) * (1 / x)^(shape + 1) * exp(-scale / x),
    NA
  )

  if (log == TRUE) {
    y <- log(y)
  }

  return(y)
}

log_prior_sigma <- function(priors, sigma) {
  dinvgamma(
    sigma^2,
    shape = priors$error$shape,
    scale = priors$error$scale,
    log = TRUE
  )
}

log_prior_smooth <- function(priors, param_grid, n_obs, n_cleaned) {
  log_prior <- dbeta(
    x = limit(param_grid[, "alpha"]),
    shape1 = priors$level$alpha,
    shape2 = priors$level$beta,
    log = TRUE
  ) +
    dbeta(
      x = limit(param_grid[, "alpha"] * param_grid[, "beta"]),
      shape1 = priors$trend$alpha,
      shape2 = priors$trend$beta,
      log = TRUE
    ) +
    dbeta(
      x = limit(param_grid[, "gamma"]),
      shape1 = priors$seasonality$alpha,
      shape2 = priors$seasonality$beta,
      log = TRUE
    ) +
    dbernoulli(
      x = ifelse(abs(param_grid[, "beta"] +
                       param_grid[, "one_minus_beta"]) < 0.0001, 0, 1),
      prob = priors$trend$prob,
      log = TRUE
    ) +
    dbernoulli(
      x = ifelse(abs(param_grid[, "gamma"] +
                       param_grid[, "one_minus_gamma"]) < 0.0001, 0, 1),
      prob = priors$seasonality$prob,
      log = TRUE
    ) +
    dbinom(x = n_cleaned, size = n_obs, prob = priors$anomaly$prob, log = TRUE)

  return(log_prior)
}

fit_sigma <- function(errors,
                      method = c("norm", "cauchy", "student")[1],
                      n_obs,
                      shape = NULL,
                      scale = NULL) {
  if (method == "norm") {
    # https://www.cs.ubc.ca/~murphyk/Papers/bayesGauss.pdf
    # Normal assuming mean is known (at 0)

    sigma_shape <- shape + n_obs / 2
    sigma_scale <- ((shape * scale + n_obs * colSums(errors^2, na.rm = TRUE)) /
                      (shape + n_obs)) / 2

    sigma <- sqrt(sigma_scale / sigma_shape)
  } else if (method == "cauchy") {
    sigma <- apply(X = errors, MARGIN = 2, FUN = stats::IQR, na.rm = TRUE) / 2
  } else if (method == "student") {
    #sigma <- apply(X = errors, MARGIN = 2, FUN = stats::IQR) / 2
    sigma <- apply(X = errors, MARGIN = 2, FUN = stats::mad, na.rm = TRUE)
  }

  return(sigma)
}

log_lik_cauchy <- function(errors, sigma) {
  colSums(
    dcauchy(
      x = errors,
      location = 0,
      scale = rep(sigma, each = dim(errors)[1]),
      log = TRUE
    ),
    na.rm = TRUE
  )
}

log_lik_norm <- function(errors, sigma) {
  colSums(
    dnorm(
      x = errors, mean = 0, sd = rep(sigma, each = dim(errors)[1]), log = TRUE
    ),
    na.rm = TRUE
  )
}

log_lik_student <- function(errors, sigma, df) {
  checkmate::assert_integerish(x = df, lower = 3)

  scaled_errors <- errors / sigma
  # instead of `sqrt(sigma^2 * (df - 2) / df)` we use `sigma` because we
  # estimate `sigma` using `mad()` which is already scaled

  colSums(
    dt(
      x = scaled_errors,
      df = df,
      log = TRUE
    ),
    na.rm = TRUE
  )
}
