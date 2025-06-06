% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assert_unique}
\alias{assert_unique}
\title{Check Duplicates}
\usage{
assert_unique(x)
}
\arguments{
\item{x}{An object to be checked.}
}
\value{
Throws an error, if any, and returns \code{x} invisibly otherwise.
}
\description{
Checks if an object contains duplicated elements.
}
\seealso{
Other checking methods: 
\code{\link{assert_constant}()},
\code{\link{assert_dim}()},
\code{\link{assert_empty}()},
\code{\link{assert_infinite}()},
\code{\link{assert_length}()},
\code{\link{assert_lower}()},
\code{\link{assert_missing}()},
\code{\link{assert_names}()},
\code{\link{assert_numeric}},
\code{\link{assert_package}()},
\code{\link{assert_square}()},
\code{\link{assert_type}()}
}
\author{
N. Frerebeau
}
\concept{checking methods}
