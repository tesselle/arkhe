% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check-attribute}
\alias{check-attribute}
\alias{assert_empty}
\alias{assert_length}
\alias{assert_lengths}
\alias{assert_dimensions}
\alias{assert_names}
\alias{assert_dimnames}
\title{Check Object Attributes}
\usage{
assert_empty(x)

assert_length(x, expected)

assert_lengths(x, expected)

assert_dimensions(x, expected)

assert_names(x, expected)

assert_dimnames(x, expected)
}
\arguments{
\item{x}{An object to be checked.}

\item{expected}{An appropriate expected value.}
}
\value{
Throws an error, if any, and returns \code{x} invisibly otherwise.
}
\description{
Check Object Attributes
}
\seealso{
Other validation methods: 
\code{\link{check-data}},
\code{\link{check-numeric}},
\code{\link{check-type}},
\code{\link{validate}()}
}
\author{
N. Frerebeau
}
\concept{validation methods}
\keyword{internal}
