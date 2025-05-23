% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/statistics.R
\docType{methods}
\name{resample_uniform}
\alias{resample_uniform}
\alias{resample_uniform-method}
\alias{resample_uniform,numeric-method}
\title{Draw Uniform Random Sample}
\usage{
resample_uniform(object, ...)

\S4method{resample_uniform}{numeric}(object, n, size = length(object), replace = FALSE, ...)
}
\arguments{
\item{object}{A \code{\link{numeric}} vector.}

\item{...}{Currently not used.}

\item{n}{A non-negative \code{\link{integer}} specifying the number of random vector
to draw.}

\item{size}{A non-negative \code{\link{integer}} specifying the sample size.}

\item{replace}{A \code{\link{logical}} scalar: should sampling be with replacement?}
}
\value{
A \code{numeric} \code{\link{matrix}} with \code{n} rows and \code{size} columns.
}
\description{
Draws a random (sub)sample (with or without replacement).
}
\examples{
## Uniform distribution
x <- rnorm(20)
resample_uniform(x, n = 10)

## Multinomial distribution
x <- sample(1:100, 20, TRUE)
resample_multinomial(x, n = 10)
}
\seealso{
Other resampling methods: 
\code{\link{bootstrap}()},
\code{\link{jackknife}()},
\code{\link{resample_multinomial}()}
}
\author{
N. Frerebeau
}
\concept{resampling methods}
