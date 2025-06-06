% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/append.R
\docType{methods}
\name{append_rownames}
\alias{append_rownames}
\alias{append_rownames-method}
\alias{append_rownames,data.frame-method}
\title{Convert Row Names to an Explicit Column}
\usage{
append_rownames(x, ...)

\S4method{append_rownames}{data.frame}(x, after = 0, remove = TRUE, var = "rownames")
}
\arguments{
\item{x}{A \code{\link{data.frame}}.}

\item{...}{Currently not used.}

\item{after}{A length-one \code{\link{numeric}} vector specifying a subscript,
after which the row names are to be appended.}

\item{remove}{A \code{\link{logical}} scalar: should the row names be removed?}

\item{var}{A \code{\link{character}} string giving the name of name of the new column.}
}
\value{
A \code{\link{data.frame}}.
}
\description{
Convert Row Names to an Explicit Column
}
\examples{
X <- data.frame(
  x = 1:5,
  y = 6:10,
  z = LETTERS[1:5]
)

## Assign column to row names
(Y <- assign_rownames(X, 3))

## Append row names to data.frame
(Z <- append_rownames(Y))
}
\seealso{
Other data preparation tools: 
\code{\link{append_column}()},
\code{\link{assign}()},
\code{\link{compact}()},
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
