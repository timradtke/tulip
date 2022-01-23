#' Initialize a grid of paramaters to perform grid search
#'
#' Currently small for development purposes.
#'
#' @export
initialize_param_grid <- function() {
  param_grid <- matrix(
    c(
      1,   0,   0,   0,   0,   0,    # Random Walk
      0,   0,   0,   0,   1,   0,    # Seasonal Random Walk
      0,   1,   0,   0,   0,   0,    # Median
      0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  # Half-half
      0.1, 0.9, 0.1, 0.9, 0.5, 0.5,
      0.1, 0.9, 0.9, 0.1, 0.5, 0.5,
      0.5, 0.5, 0.5, 0.5, 0,   0
    ),
    ncol = 6, byrow = TRUE
  )

  colnames(param_grid) <- c("alpha", "one_minus_alpha",
                            "beta", "one_minus_beta",
                            "gamma", "one_minus_gamma")

  return(param_grid)
}
