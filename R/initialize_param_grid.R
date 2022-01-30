#' Initialize a grid of paramaters to perform grid search over
#'
#' @param n_alpha Base number of `alpha` candidate values
#' @param n_beta Base number of `beta` candidate values
#' @param n_gamma Base number of `gamma` candidate values
#' @param beta_smaller_than_alpha Logical; should `beta` be constrained to
#'     always be smaller than or equal to alpha?
#' @param use_logistic Logical; should the logistic function be used to
#'     distribute the base candidate values? This is useful if the boundaries at
#'     0 and 1 should be more closely trialed than values around 0.5. If FALSE,
#'     the base candidate values are distributed linearly between 0 and 1.
#' @param logistic_limit Most extreme value provided into the logistic function;
#'     if `logistic_limit` is x, then the largest value trialed besides 1 is
#'     `1 / (1 + exp(-1 * logistic_limit))`. The default is 4, corresponding to
#'     a value of 0.982.
#'
#' @export
#' @examples
#' initialize_param_grid()[1:10, ]
initialize_param_grid <- function(n_alpha = 11,
                                  n_beta = 11,
                                  n_gamma = 11,
                                  beta_smaller_than_alpha = TRUE,
                                  use_logistic = TRUE,
                                  logistic_limit = 4) {

  if (use_logistic) {
    get_params <- function(n) {
      c(0, 1, 1 / (1 + exp(- seq(-1 * logistic_limit, logistic_limit,
                                 length.out = n - 2))))
    }
  } else {
    get_params <- function(n) seq(0, 1, length.out = n)
  }

  param_grid <- expand.grid(
    alpha = get_params(n_alpha),
    one_minus_alpha = NA,
    beta = get_params(n_beta),
    one_minus_beta = NA,
    gamma = get_params(n_gamma),
    one_minus_gamma = NA
  )

  param_grid$one_minus_alpha <- 1 - param_grid$alpha
  param_grid$one_minus_beta <- 1 - param_grid$beta
  param_grid$one_minus_gamma <- 1 - param_grid$gamma

  if (beta_smaller_than_alpha) {
    param_grid <- param_grid[param_grid$beta <= param_grid$alpha, ]
  }

  tmp_grid_no_trend <- param_grid
  tmp_grid_no_trend$beta <- 0
  tmp_grid_no_trend$one_minus_beta <- 0

  tmp_grid_no_season <- param_grid
  tmp_grid_no_season$gamma <- 0
  tmp_grid_no_season$one_minus_gamma <- 0

  tmp_grid_only_level <- param_grid
  tmp_grid_only_level$beta <- 0
  tmp_grid_only_level$one_minus_beta <- 0
  tmp_grid_only_level$gamma <- 0
  tmp_grid_only_level$one_minus_gamma <- 0

  param_grid <- as.matrix(rbind(
    tmp_grid_only_level,
    tmp_grid_no_season,
    tmp_grid_no_trend,
    param_grid
  ))

  param_grid <- unique(param_grid, MARGIN = 1)

  return(param_grid)
}
