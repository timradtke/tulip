#' Fit error / observation equation and compute loss
#'
fit_likelihood <- function(y, y_hat, m, family, param_grid) {

  k <- dim(param_grid)[1]
  errors <- y_hat - y
  sigma <- rep(NA, k)
  loglik <- rep(NA, k)


  # TODO: Write in vectorized form;
  for (i in 1:k) {
    if (family == "norm") {
      sigma[i] <- sd(x = errors[, i])
      loglik[i] <- sum(
        dnorm(x = errors[, i], mean = 0, sd = sigma[i], log = TRUE)
      )
    } else if (family == "cauchy") {
      sigma[i] <- IQR(x = errors[, i]) / 2
      loglik[i] <- sum(
        dcauchy(x = errors[, i], location = 0, scale = sigma[i], log = TRUE)
      )
    } else if (family == "nbinom") {
      # mu^2 / (variance - mu) = size
      sigma[i] <- mean(expm1(y[, i]))^2 /
        (var(expm1(y[, i])) - mean(expm1(y[, i])))
      loglik[i] <- sum(
        dnbinom(x = expm1(y[, i]), mu = y_hat[, i], size = sigma[i], log = TRUE)
      )
    }
  }

  ic <- k * log(length(dim(y)[1])) - 2 * loglik

  return(
    list(
      sigma = sigma,
      loglik = loglik,
      ic = ic
    )
  )
}

