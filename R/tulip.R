#' Fit a robust exponential smoothing model by maximum-a-posteriori estimation
#'
#' Given a univariate time series `y`, `tulip` fits an exponential smoothing
#' model using maximum-a-posteriori (MAP) estimation. Prior distributions for
#' the smoothing parameters, the error component, and the probability of
#' anomalies can be provided via `priors`. The set of available parameter
#' combinations is defined by `param_grid` over which the optimization of the
#' MAP procedure takes place. The error and trend component are additive (if
#' used), whereas the seasonal component can be additive or multiplicative.
#'
#' @param y A time series as numeric vector, may include NAs for some (but not
#'   all) of the observations
#' @param m The time series' period length in number of observations (for
#'   example, 12 for yearly seasonality with monthly observations, 1 for no
#'   seasonality, ...); does not handle multiple seasonality (e.g. weekly and
#'   yearly for daily observations)
#' @param method One of `additive` or `multiplicative`; determines whether the
#'   model uses an additive or multiplicative seasonal component. In the
#'   nomenclature of the `forecast` package, this corresponds to either the
#'   "AAA" model or the "AAM" model. The choice of `method` does not impact the
#'   level and trend components. Default is `additive`.
#'   The series `y` must be strictly positive when `method='multiplicative'` is
#'   chosen. Note that the `multiplicative` seasonality can be unstable when
#'   `y` is close to 0, especially for `y < 10`. You might want to switch to
#'   `additive` seasonality in such cases.
#' @param family The distribution used to describe the error component; must be
#'   one or multiple of `norm`, `student`, or `cauchy`. The fitting
#'   procedure is slower the more are chosen, and fastest for `norm`. Default is
#'   `c("norm", "student")` as `student` is a reliable choice when the noise is
#'   somewhat fat-tailed, while `cauchy` can go too far.
#' @param param_grid Matrix defining the grid of parameters to be trialled
#'   during grid search optimization; default parameter grid will be used if
#'   left `NULL`. Can be created using [initialize_params_random()],
#'   [initialize_params_naive()], [initialize_params_grid()], or manually.
#' @param priors List of priors on the models parameters; default priors will be
#'   used if left `NULL`. Can be created using [add_prior_error()],
#'   [add_prior_level()], [add_prior_seasonality()], [add_prior_trend()],
#'   [add_prior_anomaly()]
#' @param init_states List of initial states `l`, `b`, and `s` used to start
#'   iterative smoothing of the time series for each set of the parameters in
#'   the parameter grid. Default initialization via [initialize_states()] will
#'   be used if left `NULL`. If `s` is provided but not both `l` and `b`,
#'   `s` will be used as initial seasonal component, and [initialize_states()]
#'   will initialize `l` and `b` *given* the user-provided `s`.
#' @param seasonality_threshold During initialization of states, only use a
#'   seasonal component if more than `seasonality_threshold` share of the
#'   overall variation of the time series is explained by the seasonal
#'   component. This doesn't quite correspond to the residual variance as
#'   the variation reduction is calculated based on robust measures.
#'   Reasonable values are somewhere between 0 and 1.
#' @param remove_anomalies Logical; during fitting, anomalies can be identified
#'   and interpolated to not adversely affect the fitted states. The
#'   interpolated values are only used to fit the states. When it comes to
#'   estimating the error's standard deviation `sigma` and to measuring the
#'   likelihood, interpolated values (i.e., fitted values) are compared against
#'   the original "anomalies" so that robust distributions like Cauchy and
#'   Student are correctly attributed a higher likelihood than the Normal
#'   distribution and may be chosen to project the uncertainty due to anomalies
#'   into the future. Default is `TRUE`.
#' @param anomaly_budget Integerish (default 5); the number of anomalies that
#'   can be interpolated during fitting of state components. It can be useful to
#'   set a somewhat low hard limit on the number of possible interpolations as
#'   some parameter grid combinations may be misspecified compared to the
#'   "correct" data generating process and would therefore consider every
#'   observation an anomaly.
#' @param anomaly_budget_most_recent_k Integerish (default 1); additional budget
#'   reserved to remove anomalies from the `k` most recent observations only.
#'   Especially models with larger `alpha` and `beta` smoothing parameters
#'   adjust the forecast strongly to the most recent observation(s). But even if
#'   the smoothing parameters are small, an anomalous most recent observation(s)
#'   can have a large impact on the forecast if it is sufficiently large (or
#'   small). At the same time, it can be undesired that the forecast adjusts
#'   heavily to the one, two, or k most recent observations even if they are
#'   very different from the rest of the series. For a discussion of this, see
#'   also: Michael Bohlke-Schneider, Shubham Kapoor, Tim Januschowski (2020).
#'   *Resilient Neural Forecasting Systems*.
#'   <https://www.amazon.science/publications/resilient-neural-forecasting-systems>
#' @param min_obs_anomaly_removal Integerish (default 12); the anomaly detection
#'   relies on the fitted values' errors' standard deviation. The standard
#'   deviation is iteratively updated as the state components are fitted from
#'   the first observation to the last observation. This parameter defines after
#'   which observation of the time series there are sufficiently many
#'   observations available to reliably estimate the error's standard deviation
#'   and thus determine whether an observation should be considered an anomaly.
#'   The default of 12 is useful for monthly observations and not too low. But
#'   it also implies that any anomaly in the first 12 observations will have
#'   an impact on the estimated state components.
#' @param try_fixed_initial_fit Logical (default `FALSE`); should the initial
#'   *global* fit provided via `init_states` / [initialize_states()]
#'   be evaluated alongside the smoothed model fits? This will evaluate the fit
#'   corresponding to `init_states$l_global`, `init_states$b_global`, and
#'   `init_states$s`, resulting in fitted values `init_states$fitted_global`.
#'   In contrast, the normal smoothed model fits rely on `init_states$l`,
#'   `init_states$b`, and `init_states$s`.
#' @param check_param_grid_unique Logical (default `TRUE`); should `tulip`
#'   check that the provided `param_grid` rows are distinct (and remove
#'   duplicates otherwise)? Turn off to avoid computation if you know your
#'   `param_grid` has only distinct rows.
#'
#' @return An object of class `tulip`, a list with components:
#' \describe{
#'   \item{y_hat}{Fitted values, numeric vector of same length as `y`}
#'   \item{y}{Input time series}
#'   \item{y_cleaned}{Copy of the input time series that is used to update
#'                    the state components during model fitting. The copy is
#'                    used to achieve update behavior specific to the states
#'                    when dealing with missing values and anomalies. In
#'                    contrast to `y`, missing values and anomalies are replaced
#'                    to update the states in robust ways. Note that the update
#'                    behavior for `sigma` may differ, see also `y_na`.}
#'   \item{n_cleaned}{Number of cleaned observations, integer}
#'   \item{y_na}{Copy of the input time series that is used to update the
#'               `sigma` parameter during model fitting. The copy is used to
#'               achieve update behavior specific to `sigma` when dealing
#'               with missing values and anomalies. At the moment, anomalies
#'               are not cleaned in `y_na`, such that `sigma` will eventually
#'               adjust towards them. This behavior might change in the future.}
#'   \item{param_grid}{Set of smoothing parameters of fitted model}
#'   \item{sigma}{Fitted scale parameter of likelihood function}
#'   \item{l}{Fitted level state component}
#'   \item{b}{Fitted trend state component}
#'   \item{s}{Fitted seasonality state component}
#'   \item{l_init}{Initial level state component}
#'   \item{b_init}{Initial trend state component}
#'   \item{s_init}{Initial seasonality state component}
#'   \item{log_joint}{Value of the (log) joint distribution at the chosen
#'                    parameter values}
#'   \item{family}{The distribution family of the fitted model}
#'   \item{m}{The suspected seasonality period length}
#'   \item{method}{Either `additive` or `multiplicative` seasonal component}
#'   \item{priors}{The list of priors used during the model estimation. This
#'                 can deviate from the user-provided `priors` argument
#'                 when the user did not provide a prior for all parameters.
#'                 The returned list lists all priors that were effectively
#'                 used.}
#'   \item{comment}{A character string; missing value in the normal case, else
#'                  describing a predefined exception, e.g. in the case of a
#'                  very short input series}
#'   \item{full}{List containing all fitted models for the full parameter grid}
#' }
#'
#' @seealso [predict.tulip()], [initialize_states()], [initialize_params_grid()],
#'     [add_prior_level()], [add_prior_trend()], [add_prior_seasonality()],
#'     [add_prior_error()], [add_prior_anomaly()]
#'
#' @export
#' @examples
#'
#' fitted_model <- tulip(y = tulip::flowers$flowers, m = 12)
#'
#' print(fitted_model$family)
#' print(fitted_model$param_grid)
#'
#' plot(tulip::flowers$flowers, type = "l", col = "grey", xlab = NA)
#' points(tulip::flowers$flowers, pch = 21, bg = "black", col = "white")
#'
#' # add fitted values
#' lines(fitted_model$y_hat, col = "blue")
#'
#' # indicate observations identified as anomalies and consequently interpolated
#' idx_anomalies <- which(fitted_model$y_cleaned != tulip::flowers$flowers)
#' points(
#'   x = idx_anomalies,
#'   y = fitted_model$y_cleaned[idx_anomalies],
#'   col = "darkorange", pch = 19
#' )
#' points(
#'   x = idx_anomalies,
#'   y = fitted_model$y_cleaned[idx_anomalies],
#'   col = "white", pch = 21
#' )
#'
tulip <- function(y,
                  m,
                  method = c("additive", "multiplicative")[1],
                  family = c("norm", "student", "cauchy")[1:2],
                  param_grid = NULL,
                  priors = NULL,
                  init_states = NULL,
                  seasonality_threshold = 0.5,
                  remove_anomalies = TRUE,
                  anomaly_budget = 5,
                  anomaly_budget_most_recent_k = 1,
                  min_obs_anomaly_removal = 12,
                  try_fixed_initial_fit = FALSE,
                  check_param_grid_unique = TRUE) {

  checkmate::assert_numeric(
    x = y, any.missing = TRUE, min.len = 1, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = m, any.missing = FALSE, null.ok = FALSE, len = 1, lower = 1
  )
  checkmate::assert_choice(
    x = method, choices = c("additive", "multiplicative"), null.ok = FALSE
  )

  if (method == "multiplicative" && any(y <= 0)) {
    stop("Multiplicative seasonality (`method = 'multiplicative'`) can only be used for time series with strictly non-negative observations.") # nolint
  }

  checkmate::assert_subset(
    x = family, choices = c("norm", "cauchy", "student"), empty.ok = FALSE
  )
  checkmate::assert_matrix(
    x = param_grid, mode = "numeric", null.ok = TRUE, any.missing = FALSE,
    min.rows = 1, min.cols = 6, max.cols = 6
  )
  checkmate::assert_list(x = priors, null.ok = TRUE)
  checkmate::assert_list(x = init_states, null.ok = TRUE)
  checkmate::assert_numeric(
    x = seasonality_threshold, lower = 0, upper = 1, len = 1,
    any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_logical(
    x = remove_anomalies, len = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = anomaly_budget, lower = 0, len = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = anomaly_budget_most_recent_k, lower = 0, len = 1, any.missing = FALSE,
    null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = min_obs_anomaly_removal, lower = 2, len = 1, any.missing = FALSE,
    null.ok = FALSE
  )
  checkmate::assert_logical(
    x = try_fixed_initial_fit, len = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_logical(
    x = check_param_grid_unique, len = 1, any.missing = FALSE, null.ok = FALSE
  )

  n_y <- length(y)

  if (all(is.na(y))) {
    warning("All observations in `y` are NA. Returning NA as forecast.")
    return(default_object(y = y, y_hat = rep(NA, n_y), m = m,
                          method = method, priors = priors, comment = "all_NA"))
  }

  # Handle input time series edge cases ----

  if (length(y) == 1) {
    warning("The length of the provided `y` is 1. Returning `y` as forecast.")
    return(default_object(y = y, y_hat = y, m = m, method = method,
                          priors = priors, comment = "single_obs"))
  }
  if (isTRUE(stats::sd(y) < 0.0001)) {
    warning("The provided `y` does not vary. Using `unique(y)` as forecast.")
    return(default_object(y = y, y_hat = y, m = m, method = method,
                          priors = priors, comment = "no_variance"))
  }

  # Initialize States of the Model ----

  init_states <- dispatch_initialize_states(
    init_states = init_states,
    y = y,
    m = m,
    method = method,
    seasonality_threshold = seasonality_threshold
  )

  # Initialize Parameter Grid to Optimize Over ----

  if (is.null(param_grid)) {
    param_grid <- initialize_params_random(seed = 190323)
  } else {
    checkmate::assert_names(
      x = colnames(param_grid),
      identical.to = c("alpha", "one_minus_alpha",
                       "beta", "one_minus_beta",
                       "gamma", "one_minus_gamma")
    )
    checkmate::assert_true(all((rowSums(param_grid) - 3) < 0.001))
    checkmate::assert_true(all(param_grid <= 1 & param_grid >= 0))

    if (check_param_grid_unique) {
      param_grid_unique <- unique(x = param_grid, MARGIN = 1)
      if (nrow(param_grid_unique) < nrow(param_grid)) {
        warning("You provided a `param_grid` with duplicate rows. Running `tulip` with `unique(param_grid)` instead.") # nolint
        param_grid <- param_grid_unique
      }
    }
  }

  # Initialize Prior Distributions of Parameters ----

  # If some of the required priors were not specified by the user, they are
  # added to a (potentially partially available) `priors` list here based on
  # default values. The prior on `anomaly` and `error` is based on the data,
  # as information from the initial state is used to derive them.

  if (is.null(priors)) {
    priors <- list()
  }

  if (!("anomaly" %in% names(priors))) {
    priors <- add_prior_anomaly(
      prob = guess_anomaly_prob_from_anomaly_candidates(
        anomaly_candidates = init_states$anomaly_candidates,
        fallback = 0
      ),
      priors = priors
    )
  } else {
    assert_prior_anomaly(prior = priors$anomaly)
  }

  if (!("error" %in% names(priors))) {
    assert_priors_error_requirements(
      priors = priors,
      init_states = init_states,
      y = y
    )

    priors <- add_prior_error(
      guess = guess_error_sd_from_residuals(
        residuals = init_states$residuals_global,
        y = y,
        fallback = 1
      ),
      n = 1,
      priors = priors
    )
  } else {
    assert_prior_error(prior = priors$error)
  }

  if (!("level" %in% names(priors))) {
    priors <- add_prior_level(guess = 1/8, n = 8, priors = priors)
  } else {
    assert_prior_level(prior = priors$level)
  }

  if (!("trend" %in% names(priors))) {
    priors <- add_prior_trend(
      prob = 0.75, guess = 1/15, n = 15, priors = priors
    )
  } else {
    assert_prior_trend(prior = priors$trend)
  }

  if (!("seasonality" %in% names(priors))) {
    priors <- add_prior_seasonality(
      prob = 0.75, guess = 1/6, n = 6, priors = priors
    )
  } else {
    assert_prior_seasonality(prior = priors$seasonality)
  }

  # Fit States Over the Parameter Grid ----

  fitted_states <- fit_states_over_grid(
    y = y,
    m = m,
    method = method,
    init_states = init_states,
    param_grid = param_grid,
    remove_anomalies = remove_anomalies,
    anomaly_candidates = init_states$anomaly_candidates,
    anomaly_budget = anomaly_budget,
    anomaly_budget_most_recent_k = anomaly_budget_most_recent_k,
    min_obs_anomaly_removal = min_obs_anomaly_removal
  )

  # Add fixed initial fit to possible set of models ----

  # Sometimes things might go wrong during smoothing and the initial global fit
  # might be a better model; thus add it as one of the options to be evaluated

  if (!test_try_initial_fit_requirements(
    try_fixed_initial_fit = try_fixed_initial_fit,
    init_states = init_states,
    y = y
  )) {
    try_fixed_initial_fit <- FALSE
  }

  if (try_fixed_initial_fit) {
    fitted_states <- add_fixed_inital_fit_to_fitted_states(
      fitted_states = fitted_states,
      init_states = init_states,
      y = y,
      m = m
    )
    if (isTRUE(fitted_states$added_fixed_initial_fit)) {
      param_grid <- add_fixed_initial_fit_to_param_grid(
        param_grid = param_grid
      )
    }
  }

  # Find the Optimal Set of Parameters via Maximum-a-Posteriori ----

  joint <- measure_joint(
    param_grid = param_grid,
    priors = priors,
    family = family,
    m = m,
    y = fitted_states$y,
    y_hat = fitted_states$y_hat,
    n_cleaned = fitted_states$n_cleaned
  )

  ls_opt_idx <- fn_get_optimal_param_idx(
    log_joint = joint$log_joint,
    idx_wrap = joint$idx_wrap
  )

  # Return Results ----

  fitted_model <- list(
    y_hat = fitted_states$y_hat[, ls_opt_idx$wrapped],
    y = y,
    y_cleaned = fitted_states$y_cleaned[, ls_opt_idx$wrapped],
    n_cleaned = fitted_states$n_cleaned[ls_opt_idx$wrapped],
    y_na = fitted_states$y_na[, ls_opt_idx$wrapped],
    param_grid = param_grid[ls_opt_idx$wrapped, ],
    sigma = joint$sigma[ls_opt_idx$raw],
    l = fitted_states$l[, ls_opt_idx$wrapped],
    b = fitted_states$b[, ls_opt_idx$wrapped],
    s = fitted_states$s[, ls_opt_idx$wrapped],
    l_init = fitted_states$l_init[, ls_opt_idx$wrapped],
    b_init = fitted_states$b_init[, ls_opt_idx$wrapped],
    s_init = fitted_states$s_init[, ls_opt_idx$wrapped],
    log_joint = joint$log_joint[ls_opt_idx$raw],
    family = joint$family[ls_opt_idx$raw],
    m = m,
    method = method,
    priors = priors,
    init_states = init_states,
    comment = NA_character_,
    full = list(
      param_grid = param_grid,
      log_joint = joint$log_joint,
      family = joint$family,
      sigma = joint$sigma,
      l = fitted_states$l,
      b = fitted_states$b,
      s = fitted_states$s,
      l_init = fitted_states$l_init,
      b_init = fitted_states$b_init,
      s_init = fitted_states$s_init,
      y_hat = fitted_states$y_hat,
      y_cleaned = fitted_states$y_cleaned,
      n_cleaned = fitted_states$n_cleaned
    )
  )

  class(fitted_model) <- "tulip"
  return(fitted_model)
}

#' Return an empty `tulip` object
#'
#' @param y A time series as numeric vector, may include NAs for some of the
#'   observations
#' @param y_hat Numeric vector of fitted values
#' @param m The time series' period length in number of observations (e.g., 12
#'   for yearly seasonality with monthly observations, 1 for no seasonality, ...
#'   ); does not handle multiple seasonality (e.g. weekly and yearly for daily
#'   observations)
#' @param method One of `multiplicative` or `additive`; see also `tulip()`
#' @param priors The list of priors provided to `tulip()`
#' @param comment A string that can be used to describe non-standard return
#'   cases
#'
#' @keywords internal
default_object <- function(y, y_hat, m, method, priors, comment) {
  checkmate::assert_numeric(x = y, null.ok = FALSE)
  checkmate::assert_numeric(x = y_hat, null.ok = FALSE)
  checkmate::assert_integerish(
    x = m, len = 1, lower = 1, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_string(x = comment, na.ok = TRUE, null.ok = FALSE)

  param_grid <- matrix(NA, ncol = 6, nrow = 1)
  colnames(param_grid) <- c(
    "alpha", "one_minus_alpha",
    "beta", "one_minus_beta",
    "gamma", "one_minus_gamma"
  )

  res <- list(
    y_hat = y_hat,
    y = y,
    y_cleaned = y,
    n_cleaned = 0,
    y_na = y,
    param_grid = param_grid,
    sigma = NA,
    l = NA,
    b = NA,
    s = NA,
    l_init = NA,
    b_init = NA,
    s_init = NA,
    log_joint = 0,
    family = "norm",
    m = m,
    method = method,
    priors = priors,
    comment = comment
  )

  res$full <- res
  class(res) <- "tulip"

  return(res)
}

#' Get the index of parameters that optimize the log joint distribution
#'
#' Since we allow multiple `family`s to be evaluated at once with the same
#' grid of parameter values, we first need to identify the 'raw' index that
#' tells us the optimal `family` choice. Then we can within this family identify
#' the 'wrapped' optimum index that gives the optimal `params` choice.
#'
#' When only a single `family` is evaluated, the 'raw' and 'wrapped' indices are
#' the same.
#'
#' @param log_joint A vector of length `dim(param_grid)[1]` multiplied by the
#'     number of evaluated `family`s
#' @param idx_wrap The number of params (same for each `family`); used to
#'     indicate at which index the `log_joint` repeats the parameters
#'     for the next `family`
#'
#' @return A list of two scalar integers, named `raw` and `wrapped`
#' @keywords internal
#' @examples
#' # let's say there are 5 parameter sets being evaluated
#' log_joint_params <- rnorm(n = 5, sd = 5)
#' idx_wrap <- length(log_joint_params)
#'
#' # we evaluate the parameter sets for three different family choices, which
#' # creates 3 * 5 different sets of models to be evaluated
#' log_joint <- c(
#'   log_joint_params + rnorm(n = 1, sd = 5),
#'   log_joint_params + rnorm(n = 1, sd = 5),
#'   log_joint_params + rnorm(n = 1, sd = 5)
#' )
#'
#' ls_opt_idx <- tulip:::fn_get_optimal_param_idx(
#'   log_joint = log_joint,
#'   idx_wrap = idx_wrap
#' )
#'
#' plot(x = 1:idx_wrap,
#'      y = seq(-30, 30, length.out = idx_wrap),
#'      type = "n", xlab = "idx", ylab = "log joint")
#'
#' for (i in 1:(length(log_joint) / idx_wrap)) {
#'   lines(
#'     x = 1:idx_wrap,
#'     y = log_joint[(i*idx_wrap - 4):(i*idx_wrap)],
#'     lty = i
#'   )
#' }
#'
#' points(ls_opt_idx$wrapped, log_joint[ls_opt_idx$raw], pch = 19)
#'
fn_get_optimal_param_idx <- function(log_joint, idx_wrap) {
  checkmate::assert_numeric(
    x = log_joint, min.len = 0, any.missing = FALSE, null.ok = FALSE
  )
  checkmate::assert_integerish(
    x = idx_wrap, len = 1, lower = 1, any.missing = FALSE, null.ok = FALSE
  )

  if (length(log_joint) == 0) {
    # this might occur even if the user specification is valid-ish
    stop("Failed to optimize the joint distribution. Does your time series include too many NAs, or did you provide an empty `param_grid`?") # nolint
  }
  if (length(log_joint) %% idx_wrap != 0) {
    # this should only occur if the package-internal code is misspecified
    stop("`log_joint` must be a vector of length that is a multiple of `idx_wrap`.")
  }

  opt_idx <- which.max(log_joint)

  if (opt_idx > idx_wrap) {
    opt_idx_wrapped <- opt_idx %% idx_wrap
    if (opt_idx_wrapped == 0) {
      opt_idx_wrapped <- idx_wrap
    }
  } else {
    opt_idx_wrapped <- opt_idx
  }

  return(list(raw = opt_idx, wrapped = opt_idx_wrapped))
}

#' Safe initialization of guess for error prior
#'
#' Use the MAD of the residuals if possible, else the MAD of the time series `y`
#' else the fallback.
#'
#' @param residuals Residuals of some initial fit, usually returned by
#'   `initialize_states()`
#' @param y The time series for which we guess the standard deviation of the
#'   residual error
#' @param fallback A fallback value that is used when both `residuals` and `y`
#'   return unusable values
#'
#' @return A strictly positive scalar
#' @keywords internal
#'
guess_error_sd_from_residuals <- function(residuals, y, fallback = 1) {
  checkmate::assert_numeric(x = residuals, min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(x = y, min.len = 1, null.ok = FALSE)
  checkmate::assert_numeric(
    x = fallback, len = 1, null.ok = FALSE, any.missing = FALSE,
    lower = 0.00000001
  )

  guess <- stats::mad(x = residuals, na.rm = TRUE)
  if (is.na(guess) || guess == 0) {
    guess <- stats::sd(residuals, na.rm = TRUE)
  }
  if (is.na(guess) || guess == 0) {
    guess <- stats::mad(y, na.rm = TRUE)
  }
  if (is.na(guess) || guess == 0) {
    guess <- stats::sd(y, na.rm = TRUE)
  }
  if (is.na(guess) || guess == 0) {
    guess <- fallback
  }

  return(guess)
}

guess_anomaly_prob_from_anomaly_candidates <- function(anomaly_candidates,
                                                       fallback) {
  checkmate::assert_logical(x = anomaly_candidates, min.len = 1, null.ok = TRUE)
  checkmate::assert_numeric(
    x = fallback, len = 1, null.ok = FALSE, any.missing = FALSE,
    lower = 0, upper = 1
  )

  guess <- fallback
  if (!is.null(anomaly_candidates)) {
    guess <- mean(anomaly_candidates, na.rm = TRUE)
  }

  return(guess)
}


add_fixed_inital_fit_to_fitted_states <- function(fitted_states,
                                                  init_states,
                                                  y,
                                                  m) {

  fitted_states$y <- cbind(fitted_states$y, y)
  fitted_states$y_hat <- cbind(fitted_states$y_hat,init_states$fitted_global)
  fitted_states$n_cleaned <- c(fitted_states$n_cleaned, 0)
  fitted_states$y_cleaned <- cbind(fitted_states$y_cleaned, y)
  fitted_states$y_na <- cbind(fitted_states$y_na, y)
  fitted_states$e <- cbind(fitted_states$e, init_states$residuals_global)
  fitted_states$b <- cbind(fitted_states$b,
                           rep(init_states$b_global, nrow(fitted_states$b)))
  fitted_states$l <- cbind(fitted_states$l,
                           init_states$l_global +
                             cumsum(rep(init_states$b_global,
                                        nrow(fitted_states$b))))
  fitted_states$s <- cbind(fitted_states$s, init_states$s[-(1:m)])
  fitted_states$l_init <- cbind(fitted_states$l_init,
                                rep(init_states$l_global,
                                    nrow(fitted_states$l_init)))
  fitted_states$b_init <- cbind(fitted_states$b_init,
                                rep(init_states$b_global,
                                    nrow(fitted_states$b_init)))
  fitted_states$s_init <- cbind(fitted_states$s_init, init_states$s[1:m])

  fitted_states$added_fixed_initial_fit <- TRUE

  return(fitted_states)
}

add_fixed_initial_fit_to_param_grid <- function(param_grid) {
  param_grid <- rbind(
    param_grid,
    matrix(
      data = c(0, 1, 0, 1, 0, 1), nrow = 1,
      dimnames = list(NULL, c("alpha", "one_minus_alpha", "beta",
                              "one_minus_beta", "gamma", "one_minus_gamma"))
    )
  )

  return(param_grid)
}

test_try_initial_fit_requirements <- function(try_fixed_initial_fit,
                                               init_states,
                                               y) {

  if (!try_fixed_initial_fit || is.null(init_states)) {
    # all good because either the initial fit will anyhow not be tried, or
    # because `initialize_states()` will be used to create `init_states`
    return(TRUE)
  }

  n_obs <- length(y)

  tests <- checkmate::test_numeric(
    x = init_states$l_global, len = 1, any.missing = FALSE, null.ok = FALSE
  ) && checkmate::test_numeric(
    x = init_states$b_global, len = 1, any.missing = FALSE, null.ok = FALSE
  ) && checkmate::test_numeric(
    x = init_states$fitted_global, len = n_obs, any.missing = FALSE,
    null.ok = FALSE
  ) && test_residuals_global_given_y(
    residuals_global = init_states$residuals_global, y = y
  )

  if (!tests) {
    warning(
      "When using `try_fixed_initial_fit` and providing `init_states`, ensure that `l_global`, `b_global`, `fitted_global`, `residuals_global` are provided as part of `init_states`. For details, see `?tulip::initialize_states()`. Else, consider setting `try_fixed_initial_fit = FALSE`."
    )
  }

  return(tests)
}

#' Test whether provided residuals vector fits to provided time series
#'
#' Returns `FALSE` if the residuals vector is not of the same length as `y`,
#' or not a non-NULL numeric vector.
#'
#' @keywords internal
test_residuals_global_given_y <- function(residuals_global, y) {
  checkmate::test_numeric(
    x = residuals_global, len = length(y), any.missing = TRUE,
    null.ok = FALSE
  )
}

#' Test the provided prior on error component
#'
#' Returns `FALSE` if the provided prior on the model's error component does
#' not fulfill its requirements.
#'
#' @keywords internal
test_prior_error_in_priors <- function(priors) {
  checkmate::test_list(
    x = priors, null.ok = FALSE
  ) && test_prior_inv_gamma(prior = priors$error)
}

#' Assert that either a prior on the error component is properly specified,
#' or the initial states are not provided at all such that they are
#' automatically generated alongside a `residuals_global` vector, or
#' that a suitable `residuals_global` vector is provided that can be used
#' to derive the not provided prior distribution on the error component.
#'
#' @keywords internal
assert_priors_error_requirements <- function(priors, init_states, y) {

  if (is.null(init_states)) {
    # good because `initialize_states()` will be used to create `init_states`
    return(invisible(TRUE))
  }

  if (test_prior_error_in_priors(priors)) {
    return(invisible(TRUE))
  }

  if (test_residuals_global_given_y(
    residuals_global = init_states$residuals_global, y = y
  )) {
    return(invisible(TRUE))
  }

  stop("If `init_states` is not NULL, please provide `init_states$residuals_global`; see `?initialize_states()` for more information.") # nolint
}
