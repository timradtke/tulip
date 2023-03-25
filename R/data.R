#' Flowers time series
#'
#' A data frame containing a single synthetic time series with monthly
#' observations ranging from January 2015 through December 2021.
#'
#' The values represent some non-negative counts and have anomalies as final two
#' observations.
#'
#' @format ## `flowers`
#' A data frame with 84 rows and 2 columns:
#' \describe{
#'   \item{date}{The month formatted as date using the first day of the month}
#'   \item{flowers}{The time series observation}
#' }
#' @seealso [flowers_holdout]
#' @examples
#'
#' plot(tulip::flowers, type = "l", col = "grey")
#' points(tulip::flowers, pch = 19, cex = 0.5)
#'
"flowers"

#' Flowers time series (holdout)
#'
#' A data frame containing a single synthetic time series with monthly
#' observations ranging from January 2022 through December 2022. This is a
#' continuation of the [flowers] dataset, generated based on a single data
#' generating process. Can be used to evaluate models trained on the [flowers]
#' data.
#'
#' The values represent some non-negative counts and have anomalies as final two
#' observations.
#'
#' @format ## `flowers_holdout`
#' A data frame with 12 rows and 2 columns:
#' \describe{
#'   \item{date}{The month formatted as date using the first day of the month}
#'   \item{flowers}{The time series observation}
#' }
#' @seealso [flowers]
#' @examples
#'
#' plot(rbind(tulip::flowers, tulip::flowers_holdout), col = "white")
#' abline(v = as.Date("2021-12-15"), lty = 2)
#' lines(tulip::flowers, type = "l", col = "grey")
#' lines(tulip::flowers_holdout, type = "l", col = "grey")
#' points(tulip::flowers, pch = 19, cex = 0.5)
#' points(tulip::flowers_holdout, pch = 19, cex = 0.5)
#'
"flowers_holdout"
