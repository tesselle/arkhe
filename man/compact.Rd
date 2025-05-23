% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/compact.R
\docType{methods}
\name{compact}
\alias{compact}
\alias{compact-method}
\alias{compact_columns}
\alias{compact_columns-method}
\alias{compact_rows}
\alias{compact_rows-method}
\alias{compact,ANY-method}
\alias{compact_columns,ANY-method}
\alias{compact_rows,ANY-method}
\title{Remove Empty Rows/Columns}
\usage{
compact(x, ...)

compact_columns(x, ...)

compact_rows(x, ...)

\S4method{compact}{ANY}(x, margin = 1, na.rm = FALSE, verbose = getOption("arkhe.verbose"))

\S4method{compact_columns}{ANY}(x, na.rm = FALSE, verbose = getOption("arkhe.verbose"))

\S4method{compact_rows}{ANY}(x, na.rm = FALSE, verbose = getOption("arkhe.verbose"))
}
\arguments{
\item{x}{An \R object (should be a \code{\link{matrix}} or a \code{\link{data.frame}}).}

\item{...}{Currently not used.}

\item{margin}{A length-one \code{\link{numeric}} vector giving the subscripts which the
function will be applied over (\code{1} indicates rows, \code{2} indicates columns).}

\item{na.rm}{A \code{\link{logical}} scalar: should \code{NA} values be stripped before the
computation proceeds?}

\item{verbose}{A \code{\link{logical}} scalar: should \R report extra information
on progress?}
}
\description{
Removes empty rows/columns in an array-like object.
}
\details{
A row/column is empty if it contains only zeros (if of type \code{numeric})
or zero length character strings (if of type \code{character}).
}
\examples{
## Create a data.frame
X <- data.frame(A = 0, B = 1:5, C = 6, D = "", F = letters[1:5])
X

## Remove empty columns
compact(X, margin = 2)
}
\seealso{
Other data preparation tools: 
\code{\link{append_column}()},
\code{\link{append_rownames}()},
\code{\link{assign}()},
\code{\link{count}()},
\code{\link{detect}()},
\code{\link{discard}()},
\code{\link{get}()},
\code{\link{keep}()},
\code{\link{seek}()}
}
\author{
N. Frerebeau
}
\concept{data preparation tools}
