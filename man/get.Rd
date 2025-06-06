% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/get.R
\docType{methods}
\name{get}
\alias{get}
\alias{get_columns}
\alias{get_columns-method}
\alias{get_rows}
\alias{get_rows-method}
\alias{get_columns,ANY-method}
\alias{get_rows,ANY-method}
\title{Get Rows/Columns by Name}
\usage{
get_columns(x, ...)

get_rows(x, ...)

\S4method{get_columns}{ANY}(x, select = NULL, names = NULL, ...)

\S4method{get_rows}{ANY}(x, select = NULL, names = NULL, ...)
}
\arguments{
\item{x}{An \R object (should be a \code{\link{matrix}} or a \code{\link{data.frame}}).}

\item{...}{Further arguments to be passed to \code{select}.}

\item{select}{A \code{\link{function}} to be applied to the row/column names
(e.g. \code{\link[=startsWith]{startsWith()}}) that returns an \code{integer} or \code{logical} vector.}

\item{names}{A \code{\link{character}} vector of row/column names to look for.
Only used if \code{select} is \code{NULL}.}
}
\value{
An object of the same sort as \code{x}.
}
\description{
Returns rows/columns selected by name in an array-like object.
}
\examples{
## Seek columns
seek_columns(iris, select = startsWith, prefix = "Petal")
seek_columns(iris, names = c("Petal.Length", "Petal.Width"))

## Get columns
x <- get_columns(iris, select = startsWith, prefix = "Petal")
head(x)

x <- get_columns(iris, names = c("Petal.Length", "Petal.Width"))
head(x)
}
\seealso{
Other data preparation tools: 
\code{\link{append_column}()},
\code{\link{append_rownames}()},
\code{\link{assign}()},
\code{\link{compact}()},
\code{\link{count}()},
\code{\link{detect}()},
\code{\link{discard}()},
\code{\link{keep}()},
\code{\link{seek}()}
}
\author{
N. Frerebeau
}
\concept{data preparation tools}
