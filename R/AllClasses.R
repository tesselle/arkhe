# CLASSES DEFINITION

# ArchaeoData ==================================================================
#' Archaeological Data
#'
#' A virtual S4 class to represent archaeological data.
#' @slot sites A \code{\link{character}} vector.
#' @slot groups A \code{\link{character}} vector.
#' @section Get and set:
#'  In the code snippets below, \code{x} is an \code{ArchaeoData} object.
#'  \describe{
#'   \item{\code{get_groups(x)} and \code{set_groups(x) <- value}}{Get or set
#'   the sample names of \code{x}.}
#'  }
#' @author N. Frerebeau
#' @docType class
#' @aliases ArchaeoData-class
#' @keywords internal
.ArchaeoData <- setClass(
  Class = "ArchaeoData",
  slots = c(
    sites = "character",
    groups = "character"
  ),
  contains = "VIRTUAL"
)

# DataMatrix ===================================================================
#' Data Matrix
#'
#' S4 classes that represent a \eqn{m \times p}{m x p} matrix.
#' @note
#'  These classes inherit from \code{\link{matrix}}.
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @name DataMatrix
#' @rdname DataMatrix
#' @keywords internal
NULL

#' @aliases IntegerMatrix-class
#' @rdname DataMatrix
.IntegerMatrix <- setClass(
  Class = "IntegerMatrix",
  contains = "matrix"
)

#' @aliases NumericMatrix-class
#' @rdname DataMatrix
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = "matrix"
)

#' @aliases LogicalMatrix-class
#' @rdname DataMatrix
.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  contains = "matrix"
)

# IntegerMatrix ================================================================
# CountMatrix ------------------------------------------------------------------
#' Absolute Frequency Matrix
#'
#' An S4 class to represent an absolute frequency matrix (i.e. the number of
#' times a given datum occurs in a dataset).
#' @inheritParams base::matrix
#' @seealso \link{as_count}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @exportClass CountMatrix
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = c("IntegerMatrix", "ArchaeoData")
)

#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  mtx <- make_matrix(
    data = as_integer(data),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )

  .CountMatrix(mtx)
}

# OccurrenceMatrix -------------------------------------------------------------
#' Co-Occurrence Matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @slot total An \code{\link{integer}} giving the total number of observations.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main
#'  diagonal, which works out how many times each pairs of taxa/types occur
#'  together in at least one sample.
#' @seealso \link{as_occurrence}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases OccurrenceMatrix-class
.OccurrenceMatrix <- setClass(
  Class = "OccurrenceMatrix",
  slots = c(
    total = "integer"
  ),
  contains = "IntegerMatrix"
)

# NumericMatrix ================================================================
# AbundanceMatrix --------------------------------------------------------------
#' Relative Frequency Matrix
#'
#' An S4 class to represent a relative frequency matrix (i.e. the fraction of
#' times a given datum occurs in a dataset).
#' @inheritParams base::matrix
#' @seealso \link{as_abundance}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .AbundanceMatrix
#' @exportClass AbundanceMatrix
#' @aliases AbundanceMatrix-class
.AbundanceMatrix <- setClass(
  Class = "AbundanceMatrix",
  slots = c(
    totals = "numeric"
  ),
  contains = c("NumericMatrix", "ArchaeoData")
)

#' @export
#' @rdname AbundanceMatrix-class
AbundanceMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(
    data = as.numeric(data),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )
  totals <- rowSums(mtx)
  mtx <- mtx / totals
  mtx[is.nan(mtx)] <- 0 # Prevent division by zero

  .AbundanceMatrix(mtx, totals = totals)
}

# LogicalMatrix ================================================================
# IncidenceMatrix --------------------------------------------------------------
#' Incidence Matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams base::matrix
#' @seealso \link{as_incidence}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .IncidenceMatrix
#' @exportClass IncidenceMatrix
#' @aliases IncidenceMatrix-class
.IncidenceMatrix <- setClass(
  Class = "IncidenceMatrix",
  contains = c("LogicalMatrix", "ArchaeoData")
)

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(
    data = as.logical(data),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )

  .IncidenceMatrix(mtx)
}

# Stratigraphic ----------------------------------------------------------------
#' Stratigraphic Matrix
#'
#' An S4 class to represent a stratigraphic matrix.
#' @details
#'  A stratigraphic matrix represents directed relationships between
#'  stratigraphic units. A stratigraphic matrix is an adjacency matrix (a non
#'  symmetric square matrix with zeros on its main diagonal), suitable to build
#'  a directed acyclic graph (DAG).
#' @seealso \link{as_stratigraphy}
#' @example inst/examples/ex-stratigraphy.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases StratigraphicMatrix-class
.StratigraphicMatrix <- setClass(
  Class = "StratigraphicMatrix",
  contains = "LogicalMatrix"
)

# ArchaeoMatrix ================================================================
setClassUnion(
  name = "ArchaeoMatrix",
  members = c("CountMatrix", "AbundanceMatrix", "IncidenceMatrix")
)
