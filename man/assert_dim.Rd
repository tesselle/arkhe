% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assert_dim}
\alias{assert_dim}
\alias{assert_nrow}
\alias{assert_ncol}
\title{Check Object Dimensions}
\usage{
assert_dim(x, expected)

assert_nrow(x, expected)

assert_ncol(x, expected)
}
\arguments{
\item{x}{An object to be checked.}

\item{expected}{An appropriate expected value.}
}
\value{
Throws an error, if any, and returns \code{x} invisibly otherwise.
}
\description{
Check Object Dimensions
}
\seealso{
Other checking methods: 
\code{\link{assert_constant}()},
\code{\link{assert_empty}()},
\code{\link{assert_infinite}()},
\code{\link{assert_length}()},
\code{\link{assert_lower}()},
\code{\link{assert_missing}()},
\code{\link{assert_names}()},
\code{\link{assert_numeric}},
\code{\link{assert_package}()},
\code{\link{assert_square}()},
\code{\link{assert_type}()},
\code{\link{assert_unique}()}
}
\author{
N. Frerebeau
}
\concept{checking methods}
