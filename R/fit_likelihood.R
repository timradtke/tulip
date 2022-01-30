#' Fit error / observation equation and compute loss
#'
#' @param y Input time series to fit the model against
#' @param y_hat Fitted values after fitting the states and parameters to y
#' @param lambda Scalar used to scale the penalization of trend and seasonality
#' @param lambda Scalar used to scale the penalization of removed outliers
#' @param m Scalar indicating the suspected seasonality
#' @param family Distribution to be used as likelihood function
#' @param param_grid A matrix of possible parameter values used in grid search
#' @param l Fitted level state
#' @param b Fitted trend state
#' @param s Fitted seasonal state
#' @param l_init Initial level state
#' @param b_init Initial trend state
#' @param s_init Initial seasonal state
#' @param n_cleaned Number of values treated as outlier
#'
#' @examples
#' y <- matrix(c(1, 0, 1, -0.05), ncol = 2)
#' y_hat <- matrix(c(0.9, 0.25, 1.05, 0.05), ncol = 2)
#' m <- 12
#' family <- "norm"
#' param_grid <- initialize_param_grid()[6:7,]
#' l <- matrix(c(0.8, 0.2, 1, 0), ncol = 2)
#' b <- matrix(c(0.01, 0.01, 0.01, 0.01), ncol = 2)
#' s <- matrix(c(0.099, 0.04, 0.04, 0.04), ncol = 2)
#' l_init <- matrix(c(0.8, 0.2, 1, 0), ncol = 2)
#' b_init <- matrix(c(0.01, 0.01, 0.01, 0.01), ncol = 2)
#' s_init <- matrix(c(0.099, 0.04, 0.04, 0.04), ncol = 2)
#'
fit_likelihood <- function(y, y_hat, lambda, lambda_outlier, m, family,
                           param_grid, l, b, s, l_init, b_init, s_init,
                           n_cleaned) {

  k <- dim(param_grid)[1]
  n_obs <- dim(y)[1]
  errors <- y_hat - y

  if (family == "norm") {
    sigma <- sqrt(colMeans(errors^2) - colMeans(errors)^2)
    loglik <- colSums(
      dnorm(
        x = errors,
        mean = 0,
        sd = rep(sigma, each = dim(errors)[1]),
        log = TRUE
      )
    )
  } else if (family == "cauchy") {
    sigma <- apply(X = errors, MARGIN = 2, FUN = IQR) / 2
    loglik <- colSums(
      dcauchy(
        x = errors,
        location = 0,
        scale = rep(sigma, each = dim(errors)[1]),
        log = TRUE
      )
    )
  } else if (family == "nbinom") {
    nb_sigma <- function(y) {
      # mu^2 / (variance - mu) = size
      mean(expm1(y))^2 / (var(expm1(y)) - mean(expm1(y)))
    }
    sigma <- apply(X = y, MARGIN = 2, FUN = nb_sigma)
    loglik <- colSums(
      dnbinom(
        x = expm1(y),
        mu = y_hat,
        size = rep(sigma, each = dim(y)[1]),
        log = TRUE
      )
    )
  }

  mae <- 1 / n_obs * (colSums(abs(errors)))
  penalty <- 1 / n_obs * lambda * (sqrt(colSums(rbind(b_init[1,], b)^2)) +
                                     sqrt(colSums(rbind(s_init, s)^2)))
  penalty_outlier <- (lambda_outlier * n_cleaned / n_obs)^2
  loss <- mae + penalty + penalty_outlier

  return(
    list(
      sigma = sigma,
      loglik = loglik,
      loss = loss,
      mae = mae,
      penalty = penalty,
      penalty_outlier = penalty_outlier
    )
  )
}
