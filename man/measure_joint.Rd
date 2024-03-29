% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_joint.R
\name{measure_joint}
\alias{measure_joint}
\title{Heuristic Joint Distribution for Maximum-a-Posteriori Fit}
\usage{
measure_joint(param_grid, priors, family, m, y, y_hat, n_cleaned)
}
\arguments{
\item{param_grid}{A matrix of possible parameter values used in grid search}

\item{priors}{A named list of lists of prior parameters}

\item{family}{Distribution to be used as likelihood function}

\item{m}{Scalar indicating the suspected seasonality}

\item{y}{Input time series against which the model is fitted, used to
calculate the errors that are measured via the likelihood; this is a
matrix with number of rows equal to the number of observations in the
original input time series, and number of columns equal to the number of
parameter combinations}

\item{y_hat}{Fitted values after fitting the states and parameters to y, used
to calculate the errors that are measured via the likelihood; this is a
matrix with number of rows equal to the number of observations in the
original input time series, and number of columns equal to the number of
parameter combinations}

\item{n_cleaned}{Integer number of values treated as outlier, necessary to
evaluate the prior on anomalies.}
}
\description{
Compute the joint density for each combination of actuals, fitted values,
range of possible parameter combinations, and their prior distributions.
The result is the log joint density for each parameter combination, over
which the maximum can be chosen as optimal parameter combination in a
maximum-a-posteriori fashion.
}
\details{
The distributions for the priors are fixed except for their parameters. The
user can choose the family of the likelihood, however. Depending on the
choice of the likelihood, the standard deviation (or equivalent) will be
estimated differently based on the errors between actuals and fitted values.

Missing values are ignored and do not count into the joint density.
}
\examples{
y <- matrix(c(1, 0, 1, -0.05), ncol = 2)
y_hat <- matrix(c(0.9, 0.25, 1.05, 0.05), ncol = 2)

param_grid <- initialize_params_grid()[6:7,]

priors <- add_prior_error(
  priors = list(),
  guess = 1,
  n = 6
) |>
add_prior_level(
  guess = 0.1,
  n = 12
) |>
add_prior_trend(
  prob = 0.1,
  guess = 0.01,
  n = 6
) |>
add_prior_seasonality(
  prob = 0.75,
  guess = 0.9,
  n = 12
) |>
add_prior_anomaly(
  prob = 1/24
)

tulip:::measure_joint(
  param_grid = param_grid,
  priors = priors,
  family = c("norm", "student"),
  m = 12,
  y = y,
  y_hat = y_hat,
  n_cleaned = rep(0, nrow(param_grid))
)

}
\keyword{internal}
