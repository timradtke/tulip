
gen_test_data <- function() {
  n_params <- sample(x = c(2^(0:14), 3^(0:8), 5^(0:5)), size = 1)
  n_families <- sample(x = 1:4, size = 1)

  log_joint_params <- rnorm(n = n_params, sd = 5)

  log_joint <- rep(log_joint_params, times = n_families) +
    rep(rnorm(n = n_families, sd = 5), each = n_params)

  return(list(
    n_params = n_params,
    n_families = n_families,
    log_joint_params = log_joint_params,
    log_joint = log_joint
  ))
}

test_that("both parameter-internal and global optimal index are identified", {
  set.seed(4729)

  ls_test_data <- replicate(n = 1000, expr = gen_test_data(), simplify = FALSE)

  ls_results <- lapply(
    X = ls_test_data,
    FUN = function(x) {
      x$opt_idx <- fn_get_optimal_param_idx(
        log_joint = x$log_joint,
        idx_wrap = x$n_params
      )
      return(x)
    }
  )

  lapply(
    X = ls_results,
    FUN = function(x) {
      expect_equal(x$opt_idx$raw, which.max(x$log_joint))
      expect_equal(x$opt_idx$wrapped, which.max(x$log_joint_params))
    }
  )
})
