% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initialize_states.R
\name{get_remainder_component}
\alias{get_remainder_component}
\title{Compute the remaining component in \code{x} given that \code{component} has been
accounted for}
\usage{
get_remainder_component(x, component, method)
}
\arguments{
\item{x}{A numeric vector representing the actuals}

\item{component}{A numeric vector representing one of two components of \code{x}}

\item{method}{Are we using \code{additive} or \code{multiplicative} components?}
}
\value{
A numeric vector of the same length as \code{x} and \code{component}
}
\description{
Compute the remaining component in \code{x} given that \code{component} has been
accounted for
}
\keyword{internal}
