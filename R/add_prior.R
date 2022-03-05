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
#' @param shape TBD
#' @param rate TBD
#' @param priors A list containing other, already defined, priors. If not
#'   provided, a list will be started with `error` as entry. Else, the
#'   provided list will be extended with `error`.
#' @param verbose Should summarizing information be printed using `message()`?
#'
#' @return A list that includes a list named `error`, which is a list with
#'   `shape` and `rate` key-value pairs.
#'
#' @seealso `add_prior_trend()`, `add_prior_level()`, `add_prior_seasonality()`,
#'   `add_prior_anomaly()`
#'
#' @export
#' @examples
#' ps <- list()
#' ps <- add_prior_error(3, 2*0.75)
#' print(ps)
#'
#' # overwrites the existing entry
#' ps <- add_prior_error(3, 2*0.5)
#' print(ps)
#'
add_prior_error <- function(shape, rate, priors = NULL, verbose = FALSE) {

  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  # checkmate::assert_logical(x = plot)

  if (verbose) {
    tmp_sigma <- sqrt(1 / rgamma(n = 10000L, shape = shape, rate = rate))
    tmp_median <- round(median(tmp_sigma), 4)
    tmp_quantiles <- round(quantile(x = tmp_sigma, probs = c(0.025, 0.975)), 4)

    message(
      sprintf(
        "Prior distribution for the error component has a median at %s, and covers 95%% of probability mass with quantiles [%s, %s]", # nolint
        tmp_median, tmp_quantiles[1], tmp_quantiles[2]
      )
    )
  }

  error <- list(
    shape = shape,
    rate = rate
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
#' @param alpha The `alpha` parameter of a Beta distribution, as `shape1` in
#'   `dbeta()`. Must be larger than 0.
#' @param beta The `beta` parameter of a Beta distribution, as `shape2` in
#'   `dbeta()`. Must be larger than 0.
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
#'   prob = 0.75, alpha = 1, beta = 5, verbose = TRUE, plot = TRUE
#' )
#'
#' print(ps)
add_prior_seasonality <- function(prob,
                                  alpha,
                                  beta,
                                  priors = NULL,
                                  verbose = FALSE,
                                  plot = FALSE) {

  checkmate::assert_numeric(x = prob, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = alpha, lower = 0.00001, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = beta, lower = 0.00001, any.missing = FALSE, len = 1)
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  checkmate::assert_logical(x = plot)

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
#' @param alpha The `alpha` parameter of a Beta distribution, as `shape1` in
#'   `dbeta()`. Must be larger than 0.
#' @param beta The `beta` parameter of a Beta distribution, as `shape2` in
#'   `dbeta()`. Must be larger than 0.
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
                            alpha,
                            beta,
                            priors = NULL,
                            verbose = FALSE,
                            plot = FALSE) {

  checkmate::assert_numeric(x = prob, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = alpha, lower = 0.00001, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = beta, lower = 0.00001, any.missing = FALSE, len = 1)
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  checkmate::assert_logical(x = plot)

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
#' @param alpha The `alpha` parameter of a Beta distribution, as `shape1` in
#'   `dbeta()`. Must be larger than 0.
#' @param beta The `beta` parameter of a Beta distribution, as `shape2` in
#'   `dbeta()`. Must be larger than 0.
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
add_prior_level <- function(alpha, beta, priors = NULL, verbose = FALSE, plot = FALSE) {

  checkmate::assert_numeric(x = alpha, lower = 0.00001, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(x = beta, lower = 0.00001, any.missing = FALSE, len = 1)
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_logical(x = verbose)
  checkmate::assert_logical(x = plot)

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