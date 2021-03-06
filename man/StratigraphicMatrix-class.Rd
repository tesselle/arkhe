% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{StratigraphicMatrix-class}
\alias{StratigraphicMatrix-class}
\alias{.StratigraphicMatrix}
\title{Stratigraphic Matrix}
\description{
An S4 class to represent a stratigraphic matrix.
}
\details{
A stratigraphic matrix represents directed relationships between
stratigraphic units. A stratigraphic matrix is an adjacency matrix (a non
symmetric square matrix with zeros on its main diagonal), suitable to build
a directed acyclic graph (DAG).
}
\examples{
# Principles of Archaeological Stratigraphy, fig. 12
harris <- matrix(
  data = c(2, 1,
           3, 1,
           4, 1,
           5, 2,
           5, 3,
           5, 4,
           6, 5,
           7, 1,
           7, 6,
           8, 1,
           8, 6,
           9, 7,
           9, 8),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("lower", "upper"))
)

strati <- as_stratigraphy(harris)
}
\seealso{
\code{\link[=as_stratigraphy]{as_stratigraphy()}}

Other classes: 
\code{\link{CompositionMatrix-class}},
\code{\link{CountMatrix-class}},
\code{\link{DataMatrix}},
\code{\link{IncidenceMatrix-class}},
\code{\link{OccurrenceMatrix-class}},
\code{\link{coerce}()}
}
\author{
N. Frerebeau
}
\concept{classes}
