% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/replace.R
\docType{methods}
\name{replace_empty}
\alias{replace_empty}
\alias{replace_empty-method}
\alias{replace_empty,matrix-method}
\alias{replace_empty,data.frame-method}
\title{Replace Empty String}
\usage{
replace_empty(x, ...)

\S4method{replace_empty}{matrix}(x, value)

\S4method{replace_empty}{data.frame}(x, value)
}
\arguments{
\item{x}{An \R object (should be a \code{\link{matrix}} or a \code{\link{data.frame}}).}

\item{...}{Currently not used.}

\item{value}{A possible replacement value.}
}
\description{
Replaces empty strings.
}
\examples{
## Create a data matrix
X <- matrix(sample(LETTERS, 25, TRUE), nrow = 5, ncol = 5)

## Add empty string
k <- sample(1:25, 3, FALSE)
X[k] <- ""
X

## Remove rows with empty strings
remove_empty(X, margin = 1)

## Replace empty strings
replace_empty(X, value = "XXX")
}
\seealso{
Other data cleaning tools: 
\code{\link{clean_whitespace}()},
\code{\link{remove_Inf}()},
\code{\link{remove_NA}()},
\code{\link{remove_constant}()},
\code{\link{remove_empty}()},
\code{\link{remove_zero}()},
\code{\link{replace_Inf}()},
\code{\link{replace_NA}()},
\code{\link{replace_zero}()}
}
\author{
N. Frerebeau
}
\concept{data cleaning tools}
