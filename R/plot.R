#' Plot fitted values of an `heuristika` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its namespace
#' is available will be checked when the function is run. `ggplot2` is only
#' suggested, not a default import.
#'
#' @param object Fitted model object returned by [heuristika()]
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param show_anomalies Logical; when `TRUE` (default), observations that were
#'     treated as anomalies during model fit will be marked in orange
#' @param show_params Logical; if `TRUE` (default) then fitted params will be
#'     displayed using [ggplot2::facet_wrap()]
#'
#' @export
#' @examples
#' set.seed(4278)
#' y <- rt(100, df = 10) * 10 + 1:100
#'
#' fitted <- heuristika(y = y, m = 12, family = "norm")
#' plot_fitted(object = fitted)
#'
plot_fitted <- function(object,
                        date = NULL,
                        show_anomalies = TRUE,
                        show_params = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  checkmate::assert_logical(
    x = show_anomalies,
    len = 1,
    null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_logical(
    x = show_params,
    len = 1,
    null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_date(
    x = date,
    len = length(object$y),
    null.ok = TRUE,
    any.missing = FALSE
  )
  if (is.null(date)) {
    date <- 1:length(object$y)
  }

  anomalies <- ifelse(is.na(object$x_na), object$y, NA)

  df <- data.frame(
    date = date,
    y = object$y,
    y_hat = object$y_hat,
    anomaly = anomalies,
    family = paste0("Family: ", object$family),
    params = paste0("alpha: ", round(object$param_grid[1], 4),
                    "; beta: ", round(object$param_grid[3], 4),
                    "; gamma: ", round(object$param_grid[5], 4),
                    "; sigma: ", round(object$sigma, 4))
  )

  ggp <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = y),
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = y),
      color = "white", fill = "black", size = 3, pch = 21
    )

  if (!all(is.na(df$anomaly)) && show_anomalies) {
    ggp <- ggp +
      ggplot2::geom_point(
        ggplot2::aes(y = anomaly),
        color = "darkorange", fill = "black", size = 3.5, pch = 21,
        na.rm = TRUE,
        show.legend = TRUE
      )
  }

  ggp <- ggp +
    ggplot2::geom_point(
      ggplot2::aes(y = y_hat),
      color = "black", size = 2, pch = 21
    ) +
    ggplot2::labs(
      x = "Date",
      y = "y"
    )

  if (show_params) {
    ggp <- ggp + ggplot2::facet_wrap(~ family + params)
  }

  return(ggp)
}

#' Plot scaled components of an `heuristika` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its namespace
#' is available will be checked when the function is run. `ggplot2` is only
#' suggested, not a default import.
#'
#' @param object Fitted model object returned by [heuristika()]
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param scales One of `free` or `fixed`, passed to the `scales` argument of
#'     [ggplot2::facet_grid()]
#'
#' @export
#' @examples
#' set.seed(4278)
#' y <- rt(100, df = 10) * 10 + 1:100
#'
#' fitted <- heuristika(y = y, m = 12, family = "norm")
#' plot_components(object = fitted)
#'
plot_components <- function(object,
                            date = NULL,
                            scales = c("free", "fixed")[1]) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  checkmate::assert_choice(x = scales, choices = c("free", "fixed"))
  checkmate::assert_date(
    x = date,
    len = length(object$y),
    null.ok = TRUE,
    any.missing = FALSE
  )
  if (is.null(date)) {
    date_label <- NA
    date <- 1:length(object$y)
  } else {
    date_label <- "Date"
  }

  df_input <- data.frame(
    component = "1) Scaled Input",
    date = date,
    value = object$x,
    param = paste0("Median: ", round(object$y_median, 2),
                   "\nMAD: ", round(object$y_mad, 2))
  )

  df_level <- data.frame(
    component = "3) Level",
    date = date,
    value = object$l,
    param = paste0("alpha: ", round(object$param_grid["alpha"], 4))
  )

  df_trend <- data.frame(
    component = "4) Trend",
    date = date,
    value = object$b,
    param = paste0("beta: ", round(object$param_grid["beta"], 4))
  )

  df_seasonal <- data.frame(
    component = "5) Seasonal",
    date = date,
    value = object$s,
    param = paste0("gamma: ", round(object$param_grid["gamma"], 4))
  )

  df_error <- data.frame(
    component = "2) Error",
    date = date,
    value = object$x_hat - object$x,
    param = paste0("Family: ", object$family,
                   "\nsigma: ", round(object$sigma, 2))
  )

  df <- rbind.data.frame(
    df_input,
    df_level,
    df_trend,
    df_seasonal,
    df_error
  )

  ggp <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::facet_grid(rows = ggplot2::vars(component, param),
                        scales = scales) +
    ggplot2::geom_line(
      ggplot2::aes(y = value),
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value),
      color = "white", fill = "black", size = 1.5, pch = 21
    ) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())

  if (is.na(date_label)) {
    ggp <- ggp +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    ggp <- ggp + ggplot2::labs(x = date_label)
  }

  return(ggp)
}

#' Plot a few forecast paths of a `heuristika` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its namespace
#' is available will be checked when the function is run. `ggplot2` is only
#' suggested, not a default import.
#'
#' Note: This function will use [base::sample()] to randomly select paths that
#' are added to the plot. Set a seed if you require reproducibility.
#'
#' @param object Fitted model object returned by [heuristika()]
#' @param paths Matrix containing forecast paths as returned by [draw_paths()]
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param date_future Optional additional vector with dates in format that can
#'     be cast to `YYYY-MM-DD` with same length as `object$y`, used to create
#'     x-axis for forecast paths
#' @param n Number of paths to add to plot, a small number is recommended to be
#'    able to see the individual paths; scalar integer between 1 and 10
#' @param alpha The transparency parameter used when adding the paths to the
#'    plot, provided to [ggplot2::geom_point()] and [ggplot2::geom_line()]
#'
#' @export
#' @examples
#' set.seed(4278)
#' y <- rt(100, df = 10) * 10 + 1:100
#'
#' fitted <- heuristika(y = y, m = 12, family = "norm")
#' paths <- draw_paths(object = fitted, h = 12)
#'
#' plot_paths(object = fitted, paths = paths, n = 3)
#'
plot_paths <- function(object,
                       paths,
                       date = NULL,
                       date_future = NULL,
                       n = 5,
                       alpha = 0.75) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  checkmate::assert_matrix(x = paths, mode = "numeric")
  h <- dim(paths)[1]

  checkmate::assert_integerish(
    x = n, lower = 1, upper = 10, len = 1, null.ok = FALSE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    x = alpha,
    lower = 0,
    upper = 1,
    len = 1,
    null.ok = FALSE,
    any.missing = FALSE
  )
  checkmate::assert_date(
    x = date, len = length(object$y), null.ok = TRUE, any.missing = FALSE
  )
  checkmate::assert_date(
    x = date_future, len = h, null.ok = TRUE, any.missing = FALSE
  )

  if (is.null(date) || is.null(date_future)) {
    date_label <- NA
    date <- 1:length(object$y)
    date_future <- (length(object$y) + 1):(length(object$y) + h)
  } else {
    date_label <- "Date"
  }

  sample_idx <- sort(sample(x = 1:dim(paths)[2], size = n, replace = FALSE))

  # Pivot the wide matrix into a long data frame (without using `tidyr`)

  df_future <- data.frame(
    date = rep(date_future, times = n),
    sample_index = rep(sample_idx, each = dim(paths)[1]),
    value = NA
  )
  df_future$sample_index <- factor(df_future$sample_index, ordered = TRUE)

  for (i in seq_along(sample_idx)) {
    df_future$value[((i-1) * h + 1):(i * h)] <- paths[, sample_idx[i]]
  }

  df_input <- data.frame(
    date = date,
    value = object$y
  )

  ggp <- ggplot2::ggplot(mapping = ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = value),
      data = df_input,
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value),
      data = df_input,
      color = "white", fill = "black", size = 1.5, pch = 21
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = value, group = sample_index, color = sample_index),
      data = df_future, alpha = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value, group = sample_index, fill = sample_index),
      data = df_future,
      color = "white", size = 1.5, pch = 21, alpha = 0.75
    ) +
    ggplot2::labs(y = "Value") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_ordinal(name = "Path Index") +
    ggplot2::scale_color_ordinal(name = "Path Index")

  if (is.na(date_label)) {
    ggp <- ggp +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    ggp <- ggp + ggplot2::labs(x = date_label)
  }

  return(ggp)
}

#' Plot the marginal forecast of a `heuristika` model
#'
#' This function requires the [ggplot2][ggplot2::ggplot2-package]. Whether its namespace
#' is available will be checked when the function is run. `ggplot2` is only
#' suggested, not a default import.
#'
#' @param object Fitted model object returned by [heuristika()]
#' @param probs Numeric vector of probabilities, passed to [stats::quantile()]
#' @param date Optional additional vector with dates in format that can be cast
#'     to `YYYY-MM-DD` with same length as `object$y`, used to create x-axis
#' @param date_future Optional additional vector with dates in format that can
#'     be cast to `YYYY-MM-DD` with same length as `object$y`, used to create
#'     x-axis for forecast paths
#'
#' @export
#' @examples
#' set.seed(4278)
#' y <- rt(100, df = 10) * 10 + 1:100
#'
#' fitted <- heuristika(y = y, m = 12, family = "norm")
#' paths <- draw_paths(object = fitted, h = 12)
#'
#' plot_forecast(object = fitted, paths = paths)
#'
plot_forecast <- function(object,
                          paths,
                          date = NULL,
                          date_future = NULL,
                          show_params = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  checkmate::assert_logical(
    x = show_params, len = 1, null.ok = FALSE, any.missing = FALSE
  )
  checkmate::assert_matrix(x = paths, mode = "numeric")
  h <- dim(paths)[1]

  checkmate::assert_date(
    x = date, len = length(object$y), null.ok = TRUE, any.missing = FALSE
  )
  checkmate::assert_date(
    x = date_future, len = h, null.ok = TRUE, any.missing = FALSE
  )

  if (is.null(date) || is.null(date_future)) {
    date_label <- NA
    date <- 1:length(object$y)
    date_future <- (length(object$y) + 1):(length(object$y) + h)
  } else {
    date_label <- "Date"
  }

  params <- paste0("alpha: ", round(object$param_grid[1], 4),
                   "; beta: ", round(object$param_grid[3], 4),
                   "; gamma: ", round(object$param_grid[5], 4),
                   "; sigma: ", round(object$sigma, 2))

  df_input <- data.frame(
    date = date,
    value = object$y
  )

  df_future <- data.frame(
    date = date_future,
    y_hat_1l = apply(paths, 1, quantile, 0.5 / 12),
    y_hat_2l = apply(paths, 1, quantile, 2 / 12),
    y_hat_3l = apply(paths, 1, quantile, 3 / 12),
    y_hat_median = apply(paths, 1, quantile, 0.5),
    y_hat_3u = apply(paths, 1, quantile, 9 / 12),
    y_hat_2u = apply(paths, 1, quantile, 10 / 12),
    y_hat_1u = apply(paths, 1, quantile, 11.5 / 12)
  )

  interval_text <- "Forecast intervals at 50%, 66%, and 92%."
  if (object$m == 12) {
    interval_text <- paste0(
      interval_text,
      "\nThis corresponds to falling outside the interval for half of the year, once per quarter, once per year.") # no lint
  }

  df_input$family <- paste0("Family: ", object$family)
  df_future$family <- paste0("Family: ", object$family)
  df_input$params <- params
  df_future$params <- params

  ggp <- ggplot2::ggplot(mapping = ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = value),
      data = df_input,
      color = "grey"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = value),
      data = df_input,
      color = "white", fill = "black", size = 1.5, pch = 21
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_hat_1l, ymax = y_hat_1u),
      fill = "blue", alpha = 2/12, data = df_future
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_hat_2l, ymax = y_hat_2u),
      fill = "blue", alpha = 2/12, data = df_future
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = y_hat_3l, ymax = y_hat_3u),
      fill = "blue", alpha = 2/12, data = df_future
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = y_hat_median),
      data = df_future, color = "darkblue"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = y_hat_median),
      data = df_future,
      color = "white", size = 1.5, pch = 21, fill = "darkblue"
    ) +
    ggplot2::labs(y = "Value",
                  caption = interval_text) +
    ggplot2::theme(legend.position = "bottom")

  if (is.na(date_label)) {
    ggp <- ggp +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    ggp <- ggp + ggplot2::labs(x = date_label)
  }

  if (show_params) {
    ggp <- ggp + ggplot2::facet_wrap(~ family + params)
  }

  return(ggp)
}
