#' Forecast by drawing sample paths from a fitted object
#'
#' Generate a forecast in the form of `n` sample paths for the forecast horizon
#' `h` based on the fitted model `object`.
#'
#' The forecasts of a `tulip` model are natively represented by sample paths.
#' Instead of providing a single point forecast, or marginal quantiles, possible
#' future outcomes are summarized through samples from the joint distribution
#' across future horizons. These samples can be summarized down to marginal
#' quantiles, but they also contain the dependency between future horizons
#' that is commonly lost by forecast frameworks that solely store marginal
#' quantile forecasts.
#'
#' Depending on the chosen `family`, the samples are either based on a
#' distribution that is fitted against the model's residuals during the fitting
#' procedure; or they can be based directly on bootstrapped residuals if
#' the `family` of the `tulip` object is overwritten to be `bootstrap`. In the
#' latter case, the forecast distribution can represent asymmetries that were
#' observed in the input data. You might *not* want to use `bootstrap` when the
#' input time series is very short. In such cases, a parametric assumption can
#' help with more reasonable behavior of the error component.
#'
#' As for all exponential smoothing models, the one-step-ahead sample is fed
#' back into the model to update the state components, before the two-step-ahead
#' forecast sample is drawn given the updated state components. Through this
#' mechanism, the temporal dependencies between samples of the same sample path
#' are maintained. For example, if the initial sample is higher than average,
#' the level and trend components of the model might update to be larger, thus
#' consequently increasing chances that the next sample is again high (compared
#' to the most recently observed training observation and level).
#'
#' If the model is able to represent the input data well, then sample paths look
#' like reasonable future continuations of the input series. Averaging across
#' sample paths one can derive quantiles of the forecast distribution.
#'
#' Importantly, because the sample paths maintain the temporal dependencies,
#' one can also aggregate the forecasts *across forecast horizons* which is not
#' possible when only quantiles are returned by a model. The ability to
#' summarize arbitrary aggregates can be helpful for optimization problems that
#' use the forecast as input.
#'
#' The returned value is a matrix of dimensions `h` times `n`. Each of the `n`
#' columns represents one sample path and is a random draw from the
#' `h`-dimensional forecast distribution. See also examples below.
#'
#' Use the `postprocess_sample` argument to hook into the sampling process of
#' the `predict()` method. Provide a function that is applied on the current
#' j-step-ahead vector of samples *before* it is fed back into the model, before
#' the states are updated given those samples. This allows you to adjust the
#' data generating process--for example, to enforce non-negative count samples--
#' while doing so before the state components (level, trend, seasonality) of the
#' model are updated given the most recent sample. This can prevent the model's
#' level from drifting to zero, for example, because samples never became
#' less than zero, thus affecting subsequent samples to drift even further below
#' zero, and so on. Generated samples stay similar to the input data, allowing
#' the forecast to stay similar to the input data.
#'
#' @param object The fitted model object returned by [tulip::tulip()] of class
#'     `tulip`.
#' @param h The forecast horizon as integer number of periods.
#' @param n The integer number of sample paths to draw from the forecast
#'     distribution.
#' @param remove_anomalies Logical (default `TRUE`); during prediction, some
#'   generated samples can have the characteristics of anomalies; as such, they
#'   can be identified and interpolated to not adversely affect the state
#'   updates when predicting multiple horizos, similar to `remove_anomalies` in
#'   [tulip::tulip()]. The interpolated values are only used to fit the states.
#'   The returned sample paths will still contain the anomalous samples.
#'   Which samples are anomalous is determined based on the residuals from the
#'   original model fit.
#' @param postprocess_sample A function that is applied on a numeric vector of
#'     drawn samples for a single step-ahead before the samples are used to
#'     update the state of the model, and before outliers are removed
#'     (if applicable). By default equal to `identity()`, but could also
#'     be something like `function(x) pmax(x, 0)` to enforce a lower bound of 0,
#'     or any other transformation of interest. This works best with
#'     `family = "bootstrap"` when the applied transformation represents a
#'     feature of the data generating process that is non-Gaussian.
#'     Note that this can cause arbitrary errors caused by the author of the
#'     function provided to `postprocess_sample`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An object of class `tulip_paths` that is a list of:
#' \describe{
#'   \item{paths}{A matrix of dimensions (`h`, `n`), in which each of the `n`
#'                columns represents one sample path over the `h`-steps-ahead}
#'   \item{model}{The provided model `object`}
#' }
#'
#' @seealso [tulip::tulip()], [autoplot.tulip_paths()], [stats::predict()]
#'
#' @references
#' \describe{
#'   \item{Chapter 4.2 of: Syama Sundar Rangapuram, Matthias W. Seeger, Jan Gasthaus, Lorenzo Stella, Yuyang Wang, Tim Januschowski (2018). *Deep State Space Models for Time Series Forecasting*.}{\url{https://papers.nips.cc/paper/2018/hash/5cf68969fb67aa6082363a6d4e6468e2-Abstract.html}}
#'   \item{Chapter 6.1, for example, of: Rob J. Hyndman, Anne B. Koehler, Ralph D. Snyder, and Simone Grose (2002). *A State Space Framework for Automatic Forecasting using Exponential Smoothing Methods*.}{\url{https://doi.org/10.1016/S0169-2070(01)00110-8}}
#'   \item{Matthias Seeger, Syama Rangapuram, Yuyang Wang, David Salinas, Jan Gasthaus, Tim Januschowski, Valentin Flunkert (2017). *Approximate Bayesian Inference in Linear State Space Models for Intermittent Demand Forecasting at Scale*.}{\url{https://arxiv.org/abs/1709.07638}}
#'   \item{Matthias Seeger, David Salinas, Valentin Flunkert (2016). *Bayesian Intermittent Demand Forecasting for Large Inventories*.}{\url{https://proceedings.neurips.cc/paper/2016/file/03255088ed63354a54e0e5ed957e9008-Paper.pdf}}
#'   \item{Alexander Alexandrov, Konstantinos Benidis, Michael Bohlke-Schneider, Valentin Flunkert, Jan Gasthaus, Tim Januschowski, Danielle C. Maddix, Syama Rangapuram, David Salinas, Jasper Schulz, Lorenzo Stella, Ali Caner Türkmen, Yuyang Wang (2019). *GluonTS: Probabilistic Time Series Models in Python*.}{\url{https://arxiv.org/abs/1906.05264}}
#' }
#'
#' @export
#' @examples
#' fitted_model <- tulip(y = tulip::flowers$flowers, m = 12)
#' forecast <- predict(object = fitted_model, h = 12, n = 10000)
#'
#' # library(ggplot2)
#'
#' # visualize the marginal quantile forecast
#' # autoplot(forecast)
#'
#' # visualize a handful of sample paths directly
#' # autoplot(forecast, method = "paths")
#'
#' # let's take a closer look at the sample paths themselves
#' m_fc <- forecast$paths
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
#' # -------------------
#'
#' # An example of how the `postprocess_sample` argument can be used to
#' # enforce non-negative forecasts
#'
#' set.seed(7759)
#'
#' y <- rpois(n = 60, lambda = 1)
#'
#' ls_fit <- tulip(y = y, m = 12, family = "norm", method = "additive")
#' ls_fit$family <- "bootstrap"
#'
#' m_fc <- predict(
#'   object = ls_fit,
#'   h = 12,
#'   n = 10000,
#'   postprocess_sample = function(x) pmax(0, x)
#' )
#'
#' # library(ggplot2)
#' # autoplot(m_fc)
#'
predict.tulip <- function(object,
                          h = 12,
                          n = 10000,
                          remove_anomalies = TRUE,
                          postprocess_sample = identity,
                          ...) {

  checkmate::assert_class(x = object, classes = "tulip", null.ok = FALSE)
  checkmate::assert_list(x = object, null.ok = TRUE)
  checkmate::assert_names(
    x = names(object),
    must.include = c("l", "b", "s", "l_init", "b_init", "s_init",
                     "param_grid", "sigma", "y", "y_hat",
                     "y_na", "family", "m", "comment")
  )
  checkmate::assert_integerish(
    x = h, any.missing = FALSE, null.ok = FALSE, len = 1
  )
  checkmate::assert_integerish(
    x = n, any.missing = FALSE, null.ok = FALSE, len = 1
  )
  checkmate::assert_logical(x = remove_anomalies, len = 1, any.missing = FALSE)
  checkmate::assert_function(x = postprocess_sample)

  if (isTRUE(object$comment == "all_NA")) {
    return(
      structure(
        list(
          paths = matrix(NA, ncol = n, nrow = h),
          model = object
        ),
        class = c("tulip_paths", "matrix", "array")
      )
    )
  }
  if (isTRUE(object$comment == "no_variance")) {
    return(
      structure(
        list(
          paths = matrix(unique(object$y), ncol = n, nrow = h),
          model = object
        ),
        class = c("tulip_paths", "matrix", "array")
      )
    )
  }
  if (isTRUE(object$comment == "single_obs")) {
    return(
      structure(
        list(
          paths = matrix(unique(object$y), ncol = n, nrow = h),
          model = object
        ),
        class = c("tulip_paths", "matrix", "array")
      )
    )
  }
  if (isTRUE(object$comment == "mad_zero")) {
    warning("Using a bootstrap forecast since the MAD of the input `y` is 0.")
    return(
      structure(
        list(
          paths = matrix(sample(x = object$y, size = n * h, replace = TRUE),
                         ncol = n, nrow = h),
          model = object
        ),
        class = c("tulip_paths", "matrix", "array")
      )
    )
  }

  family <- object$family
  method <- object$method
  residuals <- stats::na.omit(object$y - object$y_hat)

  checkmate::assert_choice(
    x = method, choices = c("additive", "multiplicative"), null.ok = FALSE
  )

  # draw IID errors to be used when creating sample paths
  if (family == "cauchy") {
    m_e <- matrix(
      stats::rcauchy(n = h * n, location = 0, scale = object$sigma),
      ncol = n
    )
  } else if (family == "norm") {
    m_e <- matrix(
      stats::rnorm(n = h * n, mean = 0, sd = object$sigma),
      ncol = n
    )
  } else if (family == "student") {
    m_e <- matrix(
      stats::rt(n = h * n, df = 5) * object$sigma,
      ncol = n
    )
  } else if (family == "bootstrap") {
    m_e <- matrix(
      sample(x = residuals, size = h * n, replace = TRUE),
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

  s <- matrix(
    # outer repeat to repeat `n` times for each sample path
    rep_len(
      # inner repeat to get `m + h` state vector that will be repeated `n` times
      rep_len(s, length.out = (m + h)),
      length.out = (m + h) * n
    ),
    ncol = n, byrow = FALSE
  )

  y <- matrix(rep(NA, (m + h) * n), ncol = n)
  y_orig <- matrix(rep(NA, (m + h) * n), ncol = n)
  m_e_orig <- m_e

  if (remove_anomalies) {
    residuals_mad <- stats::mad(residuals, na.rm = TRUE)
  }

  for (i in (m+1):(m+h)) {

    y_orig[i, ] <- m_e[i-m, ] + update_prediction(
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ],
      method = method
    )

    y_orig[i, ] <- postprocess_sample(y_orig[i, ])

    if (remove_anomalies) {
      is_anomaly <- classify_anomaly_from_sigma(
        residuals = m_e[i-m, ],
        sigma = residuals_mad,
        threshold = 3
      )

      # use `y_hat` instead of `y` to continue update of parameters
      m_e[i-m, ] <- ifelse(is_anomaly, 0, m_e[i-m, ])
    }

    y[i, ] <- m_e[i-m, ] + update_prediction(
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ],
      method = method
    )

    y[i, ] <- postprocess_sample(y[i, ])

    l[i, ] <- update_level(
      alpha = alpha,
      one_minus_alpha = one_minus_alpha,
      y = y[i, ],
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ],
      method = method
    )

    b[i, ] <- update_trend(
      beta = beta,
      one_minus_beta = one_minus_beta,
      level_current = l[i, ],
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ]
    )

    s[i, ] <- update_season(
      gamma = gamma,
      one_minus_gamma = one_minus_gamma,
      y = y[i, ],
      level_previous = l[i-1, ],
      trend_previous = b[i-1, ],
      season_previous = s[i-m, ],
      method = method
    )
  }

  # drop the placeholders used for initial states
  y_orig <- y_orig[-(1:m), ]

  result <- structure(
    list(
      paths = y_orig,
      model = object
    ),
    class = "tulip_paths"
  )

  return(result)
}
