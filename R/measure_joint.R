#' Heuristic Joint Distribution for Maximum-a-Posteriori Fit
#'
#' Compute the joint density for each combination of actuals, fitted values,
#' range of possible parameter combinations, and their prior distributions.
#' The result is the log joint density for each parameter combination, over
#' which the maximum can be chosen as optimal parameter combination in a
#' maximum-a-posteriori fashion.
#'
#' The distributions for the priors are fixed except for their parameters. The
#' user can choose the family of the likelihood, however. Depending on the
#' choice of the likelihood, the standard deviation (or equivalent) will be
#' estimated differently based on the errors between actuals and fitted values.
#'
#' Missing values are ignored and do not count into the joint density.
#'
#' @param param_grid A matrix of possible parameter values used in grid search
#' @param priors A named list of lists of prior parameters
#' @param family Distribution to be used as likelihood function
#' @param m Scalar indicating the suspected seasonality
#' @param y Input time series against which the model is fitted, used to
#'     calculate the errors that are measured via the likelihood; this is a
#'     matrix with number of rows equal to the number of observations in the
#'     original input time series, and number of columns equal to the number of
#'     parameter combinations
#' @param y_hat Fitted values after fitting the states and parameters to y, used
#'     to calculate the errors that are measured via the likelihood; this is a
#'     matrix with number of rows equal to the number of observations in the
#'     original input time series, and number of columns equal to the number of
#'     parameter combinations
#' @param n_cleaned Integer number of values treated as outlier, necessary to
#'     evaluate the prior on anomalies.
#'
#' @keywords internal
#'
#' @examples
#' y <- matrix(c(1, 0, 1, -0.05), ncol = 2)
#' y_hat <- matrix(c(0.9, 0.25, 1.05, 0.05), ncol = 2)
#'
#' param_grid <- initialize_params_grid()[6:7,]
#'
#' priors <- add_prior_error(
#'   priors = list(),
#'   guess = 1,
#'   n = 6
#' ) |>
#' add_prior_level(
#'   guess = 0.1,
#'   n = 12
#' ) |>
#' add_prior_trend(
#'   prob = 0.1,
#'   guess = 0.01,
#'   n = 6
#' ) |>
#' add_prior_seasonality(
#'   prob = 0.75,
#'   guess = 0.9,
#'   n = 12
#' ) |>
#' add_prior_anomaly(
#'   prob = 1/24
#' )
#'
#' tulip:::measure_joint(
#'   param_grid = param_grid,
#'   priors = priors,
#'   family = c("norm", "student"),
#'   m = 12,
#'   y = y,
#'   y_hat = y_hat,
#'   n_cleaned = rep(0, nrow(param_grid))
#' )
#'
measure_joint <- function(param_grid,
                          priors,
                          family,
                          m,
                          y,
                          y_hat,
                          n_cleaned) {

  checkmate::assert_matrix(x = param_grid, min.rows = 1, ncols = 6)
  checkmate::assert_matrix(x = y, min.rows = 1, ncols = nrow(param_grid))
  checkmate::assert_matrix(x = y_hat, nrows = nrow(y), ncols = ncol(y))
  checkmate::assert_subset(
    x = family, choices = c("norm", "student", "cauchy"), empty.ok = FALSE
  )
  checkmate::assert_integerish(
    x = m, len = 1, lower = 1, any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = n_cleaned, len = nrow(param_grid), lower = 0, any.missing = FALSE
  )

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

  if ("norm" %in% family) {
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
  if ("cauchy" %in% family) {
    sigma_cauchy <- fit_sigma(errors = errors, n_obs = n_obs, method = "cauchy")
    l_sigma_cauchy <- log_prior_sigma(priors = priors, sigma = sigma_cauchy)
    l_cauchy <- log_lik_cauchy(errors = errors, sigma = sigma_cauchy)
  }
  if ("student" %in% family) {
    if (!("norm" %in% family)) {
      sigma_norm <- fit_sigma(
        errors = errors,
        n_obs = n_obs,
        method = "norm",
        shape = priors$error$shape,
        scale = priors$error$scale
      )
    }
    sigma_student <- fit_sigma(errors = errors, n_obs = n_obs, method = "student") # nolint
    l_sigma_student <- log_prior_sigma(priors = priors, sigma = sigma_norm)
    l_student <- log_lik_student(errors = errors, sigma = sigma_student, df = 5)
  }

  log_joint <- l_smooth
  idx_wrap <- length(log_joint)

  family_joint <- c()
  sigma <- c()
  log_joint_full <- c()

  if ("norm" %in% family) {
    family_joint <- rep("norm", length(log_joint))
    sigma <- sigma_norm
    log_joint_full <- log_joint + l_sigma_norm + l_norm
  }
  if ("cauchy" %in% family) {
    family_joint <- c(family_joint, rep("cauchy", length(log_joint)))
    sigma <- c(sigma, sigma_cauchy)
    log_joint_full <- c(log_joint_full, log_joint + l_sigma_cauchy + l_cauchy)
  }
  if ("student" %in% family) {
    family_joint <- c(family_joint, rep("student", length(log_joint)))
    sigma <- c(sigma, sigma_student)
    log_joint_full <- c(log_joint_full, log_joint + l_sigma_student + l_student)
  }

  return(
    list(
      sigma = sigma,
      log_joint = log_joint_full,
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
  log_prior <- stats::dbeta(
    x = limit(param_grid[, "alpha"]),
    shape1 = priors$level$alpha,
    shape2 = priors$level$beta,
    log = TRUE
  ) +
    stats::dbeta(
      x = limit(param_grid[, "alpha"] * param_grid[, "beta"]),
      shape1 = priors$trend$alpha,
      shape2 = priors$trend$beta,
      log = TRUE
    ) +
    stats::dbeta(
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
    stats::dbinom(
      x = n_cleaned,
      size = n_obs,
      prob = priors$anomaly$prob, log = TRUE
    )

  return(log_prior)
}

fit_sigma <- function(errors,
                      method = c("norm", "cauchy", "student")[1],
                      n_obs,
                      shape = NULL,
                      scale = NULL,
                      lower = 1e-10) {
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
    sigma <- apply(X = errors, MARGIN = 2, FUN = stats::mad, na.rm = TRUE)
  }

  # set a lower bound on sigma to avoid issues in `dcauchy`, `dnorm`, etc.
  # due to zero-valued scale parameter
  sigma <- pmax(lower, sigma)

  return(sigma)
}

log_lik_cauchy <- function(errors, sigma) {
  colSums(
    stats::dcauchy(
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
    stats::dnorm(
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
    stats::dt(
      x = scaled_errors,
      df = df,
      log = TRUE
    ),
    na.rm = TRUE
  )
}
