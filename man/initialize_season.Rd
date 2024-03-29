% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialize_states.R
\name{initialize_season}
\alias{initialize_season}
\title{Initialize the seasonal state given actuals and previous fitted}
\usage{
initialize_season(x, fitted, m, method, threshold)
}
\arguments{
\item{x}{Numeric vector of actuals}

\item{fitted}{Numeric vector of fitted values of a level and trend component}

\item{m}{The period length of the seasonal component}

\item{method}{Are we using an \code{additive} or a \code{multiplicative} seasonal
component?}

\item{threshold}{A value between 0 and 1; return a neutral default seasonal
component when the seasonal component's reduction of variance in the
residuals is less than the threshold}
}
\description{
Initialize the seasonal state given actuals and previous fitted
}
\examples{
x <- rt(n = 55, df = 3) + 5 * cospi((1:55) / 6)
x[seq_along(x) \%\% 12 == 0] <- NA_real_

plot(x, pch = 19)
lines(x)

fitted <- rep(median(x, na.rm = TRUE), length(x))

s <- tulip:::initialize_season(
  x = x,
  fitted = fitted,
  m = 12,
  method = "multiplicative",
  threshold = 0.5
)

lines(s * fitted, col = "blue", lty = 2)

}
\keyword{internal}
