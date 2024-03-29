% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/remove.R
\docType{methods}
\name{remove_constant}
\alias{remove_constant}
\alias{remove_constant-method}
\alias{remove_constant,ANY-method}
\title{Remove Constant Columns}
\usage{
remove_constant(x, ...)

\S4method{remove_constant}{ANY}(x, na.rm = FALSE, verbose = getOption("arkhe.verbose"))
}
\arguments{
\item{x}{An \R object (should be a \code{\link{matrix}} or a \code{\link{data.frame}}).}

\item{...}{Currently not used.}

\item{na.rm}{A \code{\link{logical}} scalar: should \code{NA} values be stripped before the
computation proceeds?}

\item{verbose}{A \code{\link{logical}} scalar: should \R report extra information
on progress?}
}
\description{
Remove Constant Columns
}
\examples{
## Create a data.frame
X <- data.frame(A = 1, B = 1:3)
X

remove_constant(X)

## Add NA
X[1, 1] <- NA
remove_constant(X)
remove_constant(X, na.rm = TRUE)
}
\seealso{
Other data cleaning tools: 
\code{\link{clean_whitespace}()},
\code{\link{remove_Inf}()},
\code{\link{remove_NA}()},
\code{\link{remove_empty}()},
\code{\link{remove_zero}()},
\code{\link{replace_Inf}()},
\code{\link{replace_NA}()},
\code{\link{replace_empty}()},
\code{\link{replace_zero}()}
}
\author{
N. Frerebeau
}
\concept{data cleaning tools}
