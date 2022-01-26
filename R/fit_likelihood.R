#' Fit error / observation equation and compute loss
#'
# y <- matrix(c(1, 0, 1, -0.05), ncol = 2)
# y_hat <- matrix(c(0.9, 0.25, 1.05, 0.05), ncol = 2)
# m <- 12
# family <- "norm"
# param_grid <- initialize_param_grid()[6:7,]
# l <- matrix(c(0.8, 0.2, 1, 0), ncol = 2)
# b <- matrix(c(0.01, 0.01, 0.01, 0.01), ncol = 2)
# s <- matrix(c(0.099, 0.04, 0.04, 0.04), ncol = 2)
# l_init <- matrix(c(0.8, 0.2, 1, 0), ncol = 2)
# b_init <- matrix(c(0.01, 0.01, 0.01, 0.01), ncol = 2)
# s_init <- matrix(c(0.099, 0.04, 0.04, 0.04), ncol = 2)
fit_likelihood <- function(y, y_hat, lambda, m, family, param_grid,
                           l, b, s, l_init, b_init, s_init) {

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
    sigma <- apply(X = errors, MARGIN = 2, FUN = IQR)
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
  loss <- mae + penalty

  # loss <- k * log(length(dim(y)[1])) - 2 * loglik

  return(
    list(
      sigma = sigma,
      loglik = loglik,
      loss = loss,
      mae = mae,
      penalty = penalty
    )
  )
}
