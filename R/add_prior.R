#' Add a prior on the probability of an anomaly
#'
#' @param prob The `prob` parameter for a Binomial distribution, as in
#'   `dbinom()`. The corresponding `size` parameter of `dbinom` will be the
#'   length of the time series.
#' @param priors A list containing other, already defined, priors. If not
#'   provided, a list will be started with `anomaly` as entry. Else, the
#'   provided list will be extended with `anomaly`.
#'
#' @return A list that includes a list named `anomaly`, which is a list with a
#'   `prob` key-value pair.
#'
#' @seealso `add_prior_trend()`, `add_prior_level()`, `add_prior_error()`,
#'   `add_prior_seasonality()`
#'
#' @export
#' @examples
#' ps <- list()
#' ps <- add_prior_anomaly(prob = 0.01)
#' print(ps)
#'
#' # overwrites the existing entry
#' ps <- add_prior_anomaly(prob = 0.05)
#' print(ps)
#'
add_prior_anomaly <- function(prob, priors = NULL) {
  checkmate::assert_numeric(x = prob, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  checkmate::assert_list(x = priors, null.ok = TRUE)

  anomaly <- list(
    prob = prob
  )

  if (is.null(priors)) {
    priors <- list(anomaly = anomaly)
  } else {
    checkmate::assert_list(x = priors)
    priors$anomaly <- anomaly
  }

  return(priors)
}

#' Add a prior on the standard deviation of the error component
#'
#' Defines a prior using an Inverse Gamma distribution on the variance of
#' the unexplained error component of the model.
#'
#' Since it's usually easier to think in terms of standard deviation, `guess`
#' is at the scale of the error's standard deviation. Both `guess` and `n` are
#' translated into the `shape` and `scale` parameters of the Inverse Gamma
#' distribution.
#'
#' When thinking about this prior, frame your assumptions in terms of a sample
#' of `n` observed errors with sample standard deviation `guess`.
#'
#' @param guess Which standard deviation do you expect?
#' @param n How much weight (n terms of observations) do you want to give your
#'   prior assumption? The larger this value, the more data observations it will
#'   take to wash out your prior distribution in case it is misspecified.
#' @param priors A list containing other, already defined, priors. If not
#'   provided, a list will be started with `error` as entry. Else, the
#'   provided list will be extended with `error`.
#' @param verbose Should summarizing information be printed using `message()`?
#' @param plot Should a simple plot of the implied Inverse Gamma density be
#'   drawn?
#'
#' @return A list that includes a list named `error`, which is a list with
#'   `shape` and `rate` key-value pairs.
#'
#' @seealso `add_prior_trend()`, `add_prior_level()`, `add_prior_seasonality()`,
#'   `add_prior_anomaly()`
#'
#' @references
#'   https://en.wikipedia.org/wiki/Inverse-gamma_distribution#Probability_density_function
#'
#' @export
#' @examples
#' ps <- list()
#' ps <- add_prior_error(n = 3, guess = 0.5)
#' print(ps)
#'
#' # overwrites the existing entry
#' ps <- add_prior_error(n = 3, guess = 0.5)
#' print(ps)
#'
#' # print summary information
#' add_prior_error(n = 3, guess = 0.5, verbose = TRUE)
#'
#' # plot the implied density function with indicator for the mean
#' add_prior_error(n = 3, guess = 0.5, plot = TRUE)
#' add_prior_error(n = 2, guess = 0.5, plot = TRUE)
#' add_prior_error(n = 3, guess = 2, plot = TRUE)
#' add_prior_error(n = 1, guess = 1, plot = TRUE)
#'
#' # the prior might be easier to judge on the scale of the standard deviation
#' add_prior_error(n = 1, guess = 0.5, plot = TRUE)
#' add_prior_error(n = 1, guess = 0.5, plot = TRUE, show_sigma = TRUE)
#'
add_prior_error <- function(guess,
                            n,
                            priors = NULL,
                            verbose = FALSE,
                            plot = FALSE,
                            show_sigma = TRUE) {

  checkmate::assert_numeric(x = n, len = 1, lower = 0)
  checkmate::assert_numeric(x = guess, len = 1, lower = 0)
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose, len = 1, null.ok = FALSE)
  checkmate::assert_logical(x = plot, len = 1, null.ok = FALSE)
  checkmate::assert_logical(x = show_sigma, len = 1, null.ok = FALSE)

  shape <- n / 2
  scale <- guess^2 * n / 2

  tmp_mode <- round(scale / (shape + 1), 4)
  tmp_mean <- NA
  tmp_sd <- NA
  if (shape > 2) {
    tmp_sd <- round(sqrt(scale^2 / ((shape - 1)^2 * (shape - 2))), 4)
  }
  if (shape > 1) {
    tmp_mean <- round(scale / (shape - 1), 4)
  }

  if (verbose) {
    msg_main <- sprintf(
      "The equivalent Inverse-Gamma distribution on the error variance has shape %s and scale %s.",
      round(shape, 6), round(scale, 6)
    )

    if (shape <= 1) {
      msg <- sprintf(
        "Its mode is at %s, with undefined mean and variance.",
        tmp_mode
      )
    } else if (shape <= 2) {
      msg <- sprintf(
        "Its mode is at %s and mean is at %s, with undefined variance.",
        tmp_mode, tmp_mean
      )
    } else {
      msg <- sprintf(
        "The distribution has its mode at %s, mean at %s, and a standard deviation of %s.",
        tmp_mode, tmp_mean, tmp_sd
      )
    }

    message(paste0(msg_main, "\n", msg))
  }

  if (plot) {
    plot_invgamma(
      shape = shape, scale = scale, xlim = c(0, 2), show_sigma = show_sigma
    )
  }

  error <- list(
    shape = shape,
    scale = scale
  )

  if (is.null(priors)) {
    priors <- list(error = error)
  } else {
    checkmate::assert_list(x = priors)
    priors$error <- error
  }

  return(priors)
}

#' Add a prior on the seasonal component
#'
#' The Beta distribution defined via `alpha` and `beta` is the prior on the
#' \eqn{\gamma} parameter of the estimated model. For example, if we expect that
#' the seasonal component should only update slowly over time and thus only use
#' a small part of the error component, then a small `alpha` and larger `beta`
#' make sense.
#'
#' The mean of the Beta distribution is \eqn{\mu = \alpha / (\alpha + \beta)}.
#' If `alpha` is smaller or equal than 1, than the density will be highest at 0.
#'
#' @param prob Probability that the time series has a seasonality, and that
#'   therefore a seasonal component should be included in the model. This
#'   parameterizes a Bernoulli distribution and should thus be a value in (0,1).
#' @param guess Which \eqn{\gamma} parameter do you expect?
#' @param n How much weight (n terms of observations) do you want to give your
#'   prior assumption? The larger this value, the more data observations it will
#'   take to wash out your prior distribution in case it is misspecified.
#' @param priors A list containing other, already defined, priors. If not
#'   provided, a list will be started with `seasonality` as entry. Else, the
#'   provided list will be extended with `seasonality`.
#' @param verbose Should summarizing information be printed using `message()`?
#' @param plot Should a simple plot of the implied Beta density be drawn?
#'
#' @return A list that includes a list named `seasonality`, which is a list with
#'   `prob`, `alpha`, and `beta` key-value pairs.
#'
#' @seealso `add_prior_trend()`, `add_prior_level()`, `add_prior_error()`,
#'   `add_prior_anomaly()`
#'
#' @export
#' @examples
#' ps <- add_prior_seasonality(
#'   prob = 0.75, n = 6, guess = 1/6, verbose = TRUE, plot = TRUE
#' )
#'
#' print(ps)
add_prior_seasonality <- function(prob,
                                  guess,
                                  n,
                                  priors = NULL,
                                  verbose = FALSE,
                                  plot = FALSE) {

  checkmate::assert_numeric(
    x = prob, lower = 0, upper = 1, any.missing = FALSE, len = 1
  )
  checkmate::assert_numeric(
    x = n, lower = 0.00001, any.missing = FALSE, len = 1
  )
  checkmate::assert_numeric(
    x = guess, lower = 1e-15, upper = 1 - 1e-15, any.missing = FALSE, len = 1
  )
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  checkmate::assert_logical(x = plot)

  alpha <- guess * n
  beta <- n - alpha

  if (verbose) {
    tmp_quantiles <- round(
      qbeta(c(0.05, 0.5, 0.95), shape1 = alpha, shape2 = beta), 2
    )

    message(
      sprintf(
        "The time series is expected to be seasonal with probability %s%%.",
        100 * prob
      )
    )
    message(
      sprintf(
        "The prior distribution for the seasonal component's share of the error has a median at %s, and covers 95%% of probability mass with quantiles [%s, %s]", # nolint
        tmp_quantiles[2], tmp_quantiles[1], tmp_quantiles[3]
      )
    )
  }

  if (plot) {
    plot_beta(alpha = alpha, beta = beta, xlab = "gamma")
  }

  seasonality <- list(
    prob = prob,
    alpha = alpha,
    beta = beta
  )

  if (is.null(priors)) {
    priors <- list(seasonality = seasonality)
  } else {
    checkmate::assert_list(x = priors)
    priors$seasonality <- seasonality
  }

  return(priors)
}

#' Add a prior on the local-linear trend component
#'
#' The Beta distribution defined via `alpha` and `beta` is the prior on the
#' \eqn{\alpha} and \eqn{\beta} of the estimated model. For example, if we
#' expect that the trend component should only update slowly over time and thus
#' only use a small part of the error component, then a small `alpha` and larger
#' `beta` make sense.
#'
#' Note: The trend component is updated via
#' \eqn{\alpha \cdot \beta \cdot \epsilon_t}. The prior on \eqn{\beta} is
#' implicitly defined via the `add_prior_level()` and `add_prior_trend()`.
#'
#' The mean of the Beta distribution is \eqn{\mu = \alpha / (\alpha + \beta)}.
#' If `alpha` is smaller or equal than 1, than the density will be highest at 0.
#'
#' @param prob Probability that the time series has a local-linear trend, and
#'   that therefore a trend component should be included in the model. This
#'   parameterizes a Bernoulli distribution and should thus be a value in (0,1).
#' @param guess Which \eqn{\alpha \cdot \beta} parameter do you expect?
#' @param n How much weight (n terms of observations) do you want to give your
#'   prior assumption? The larger this value, the more data observations it will
#'   take to wash out your prior distribution in case it is misspecified.
#' @param priors A list containing other, already defined, priors. If not
#'   provided, a list will be started with `trend` as entry. Else, the
#'   provided list will be extended with `trend`.
#' @param verbose Should summarizing information be printed using `message()`?
#' @param plot Should a simple plot of the implied Beta density be drawn?
#'
#' @return A list that includes a list named `trend`, which is a list with
#'   `prob`, `alpha`, and `beta` key-value pairs.
#'
#' @seealso `add_prior_seasonality()`, `add_prior_level()`, `add_prior_error()`,
#'   `add_prior_anomaly()`
#'
#' @export
#' @examples
#' ps <- add_prior_trend(
#'   prob = 0.75, alpha = 1, beta = 14, verbose = TRUE, plot = TRUE
#' )
#'
#' print(ps)
add_prior_trend <- function(prob,
                            guess,
                            n,
                            priors = NULL,
                            verbose = FALSE,
                            plot = FALSE) {

  checkmate::assert_numeric(
    x = prob, lower = 0, upper = 1, any.missing = FALSE, len = 1
  )
  checkmate::assert_numeric(
    x = n, lower = 0.00001, any.missing = FALSE, len = 1
  )
  checkmate::assert_numeric(
    x = guess, lower = 1e-15, upper = 1 - 1e-15, any.missing = FALSE, len = 1
  )
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  checkmate::assert_logical(x = plot)

  alpha <- guess * n
  beta <- n - alpha

  if (verbose) {
    tmp_quantiles <- round(
      qbeta(c(0.05, 0.5, 0.95), shape1 = alpha, shape2 = beta), 2
    )
    message(
      sprintf(
        "The time series is expected have a local trend with probability %s%%.",
        100 * prob
      )
    )
    message(
      sprintf(
        "The prior distribution for the trend component's share of the level's share of the error has a median at %s, and covers 95%% of probability mass with quantiles [%s, %s]", # nolint
        tmp_quantiles[2], tmp_quantiles[1], tmp_quantiles[3]
      )
    )
  }

  if (plot) {
    plot_beta(alpha = alpha, beta = beta, xlab = "alpha * beta")
  }

  trend <- list(
    prob = prob,
    alpha = alpha,
    beta = beta
  )

  if (is.null(priors)) {
    priors <- list(trend = trend)
  } else {
    checkmate::assert_list(x = priors)
    priors$trend <- trend
  }

  return(priors)
}

#' Add a prior on the level component
#'
#' The Beta distribution defined via `alpha` and `beta` is the prior on the
#' \eqn{\alpha} parameter of the estimated model. For example, if we expect that
#' the level of the time series varies slowly over time, and thus only a
#' small part of the error component is used to update the level at each time
#' point, then a small `alpha` and larger `beta` make sense.
#'
#' Note: In contrast to the trend and seasonal components, we don't specify an
#' "inclusion" probability `prob` on the level component. The level component
#' is always used.
#'
#' The mean of the Beta distribution is \eqn{\mu = \alpha / (\alpha + \beta)}.
#' If `alpha` is smaller or equal than 1, than the density will be highest at 0.
#'
#' Values of `alpha` closer to 0 imply a non-fluctating i.i.d. level component,
#' while values of `alpha` closer to 1 imply a more random-walk-like behavior.
#'
#' @param guess Which \eqn{\alpha} parameter do you expect?
#' @param n How much weight (n terms of observations) do you want to give your
#'   prior assumption? The larger this value, the more data observations it will
#'   take to wash out your prior distribution in case it is misspecified.
#' @param priors A list containing other, already defined, priors. If not
#'   provided, a list will be started with `level` as entry. Else, the
#'   provided list will be extended with `level`.
#' @param verbose Should summarizing information be printed using `message()`?
#' @param plot Should a simple plot of the implied Beta density be drawn?
#'
#' @return A list that includes a list named `level`, which is a list with
#'   `prob`, `alpha`, and `beta` key-value pairs.
#'
#' @seealso `add_prior_seasonality()`, `add_prior_trend()`, `add_prior_error()`,
#'   `add_prior_anomaly()`
#'
#' @export
#' @examples
#' ps <- add_prior_level(
#'   alpha = 1, beta = 7, verbose = TRUE, plot = TRUE
#' )
#'
#' print(ps)
#'
add_prior_level <- function(guess,
                            n,
                            priors = NULL,
                            verbose = FALSE,
                            plot = FALSE) {

  checkmate::assert_numeric(
    x = n, lower = 0.00001, any.missing = FALSE, len = 1
  )
  checkmate::assert_numeric(
    x = guess, lower = 1e-15, upper = 1 - 1e-15, any.missing = FALSE, len = 1
  )
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  checkmate::assert_logical(x = plot)

  alpha <- guess * n
  beta <- n - alpha

  if (verbose) {
    tmp_quantiles <- round(
      qbeta(c(0.05, 0.5, 0.95), shape1 = alpha, shape2 = beta), 2
    )
    message(
      sprintf(
        "The prior distribution for the level's share of the error has a median at %s, and covers 95%% of probability mass with quantiles [%s, %s]", # nolint
        tmp_quantiles[2], tmp_quantiles[1], tmp_quantiles[3]
      )
    )
  }

  if (plot) {
    plot_beta(alpha = alpha, beta = beta, xlab = "alpha")
  }

  level <- list(
    alpha = alpha,
    beta = beta
  )

  if (is.null(priors)) {
    priors <- list(level = level)
  } else {
    checkmate::assert_list(x = priors)
    priors$level <- level
  }

  return(priors)
}

plot_beta <- function(alpha, beta, xlab) {
  plot(
    x = seq(from = 0.0001, to = 0.9999, length.out = 1000),
    y = dbeta(
      x = seq(from = 0.0001, to = 0.9999, length.out = 1000),
      shape1 = alpha, shape2 = beta
    ),
    type = "l",
    xlab = xlab,
    ylab = ""
  ); abline(v = 0.5, lty = 3)
}

plot_invgamma <- function(shape,
                          scale,
                          xlim,
                          xlab,
                          length.out = 10000,
                          show_sigma = FALSE) {
  x <- seq(from = xlim[1], to = xlim[2], length.out = length.out)
  if (show_sigma) {
    x_var <- sqrt(x)
    xlab <- "sigma"

  } else {
    x_var <- x
    xlab <- "sigma^2"
  }
  plot(
    x = x_var,
    y = dinvgamma(
      x = x, shape = shape, scale = scale
    ),
    type = "l",
    xlab = xlab,
    ylab = ""
  )
  if (shape > 1) {
    # add indicator for mean when it exists
    if (show_sigma) {
      abline(v = sqrt(scale / (shape - 1)), lty = 3)
    } else {
      abline(v = scale / (shape - 1), lty = 3)
    }
  }
}
