# CLASSES DEFINITION

# AbundanceMatrix ==============================================================
#' Archaeological Data
#'
#' A virtual S4 class to represent archaeological data.
#' @slot samples A [`character`] vector.
#' @slot groups A [`character`] vector.
#' @section Get and set:
#'  In the code snippets below, `x` is an `AbundanceMatrix` object.
#'  \describe{
#'   \item{`get_samples(x)` and `get_samples(x) <- value`}{Get or set
#'   the sample names of `x`.}
#'   \item{`get_groups(x)` and `set_groups(x) <- value`}{Get or set
#'   the groups of `x`.}
#'  }
#' @author N. Frerebeau
#' @docType class
#' @aliases AbundanceMatrix-class
#' @keywords internal
.AbundanceMatrix <- setClass(
  Class = "AbundanceMatrix",
  slots = c(
    samples = "character",
    groups = "character"
  ),
  contains = "VIRTUAL"
)

# DataMatrix ===================================================================
#' Data Matrix
#'
#' S4 classes that represent a \eqn{m \times p}{m x p} matrix.
#' @note
#'  These classes inherit from [`matrix`].
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @name DataMatrix
#' @rdname DataMatrix
#' @keywords internal
NULL

#' @exportClass IntegerMatrix
#' @aliases IntegerMatrix-class
#' @rdname DataMatrix
.IntegerMatrix <- setClass(
  Class = "IntegerMatrix",
  contains = "matrix"
)

#' @exportClass NumericMatrix
#' @aliases NumericMatrix-class
#' @rdname DataMatrix
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = "matrix"
)

#' @exportClass LogicalMatrix
#' @aliases LogicalMatrix-class
#' @rdname DataMatrix
.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  contains = "matrix"
)

# IntegerMatrix ================================================================
## CountMatrix -----------------------------------------------------------------
#' Absolute Frequency Matrix
#'
#' An S4 class to represent an absolute frequency matrix (i.e. the number of
#' times a given datum occurs in a dataset).
#' @inheritParams base::matrix
#' @seealso [as_count()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @exportClass CountMatrix
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = c("IntegerMatrix", "AbundanceMatrix")
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
  spl <- rownames(mtx) %||% character(0)

  .CountMatrix(mtx, samples = spl)
}

## OccurrenceMatrix ------------------------------------------------------------
#' Co-Occurrence Matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @slot total An [`integer`] giving the total number of observations.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main
#'  diagonal, which works out how many times each pairs of taxa/types occur
#'  together in at least one sample.
#' @seealso [as_occurrence()]
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
## CompositionMatrix -----------------------------------------------------------
#' Relative Frequency Matrix
#'
#' An S4 class to represent a relative frequency matrix (i.e. the fraction of
#' times a given datum occurs in a dataset).
#' @slot total A [`numeric`] vector giving the absolute row sums.
#' @inheritParams base::matrix
#' @seealso [as_composition()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .CompositionMatrix
#' @exportClass CompositionMatrix
#' @aliases CompositionMatrix-class
.CompositionMatrix <- setClass(
  Class = "CompositionMatrix",
  slots = c(
    total = "numeric"
  ),
  contains = c("NumericMatrix", "AbundanceMatrix")
)

#' @export
#' @rdname CompositionMatrix-class
CompositionMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
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
  spl <- rownames(mtx) %||% character(0)
  totals <- rowSums(mtx, na.rm = TRUE)
  mtx <- mtx / totals
  # mtx[is.nan(mtx)] <- 0 # Prevent division by zero

  .CompositionMatrix(mtx, samples = spl, total = totals)
}

# LogicalMatrix ================================================================
## IncidenceMatrix -------------------------------------------------------------
#' Incidence Matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams base::matrix
#' @seealso [as_incidence()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .IncidenceMatrix
#' @exportClass IncidenceMatrix
#' @aliases IncidenceMatrix-class
.IncidenceMatrix <- setClass(
  Class = "IncidenceMatrix",
  contains = c("LogicalMatrix", "AbundanceMatrix")
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
  spl <- rownames(mtx) %||% character(0)

  .IncidenceMatrix(mtx, samples = spl)
}

## Stratigraphic ---------------------------------------------------------------
#' Stratigraphic Matrix
#'
#' An S4 class to represent a stratigraphic matrix.
#' @details
#'  A stratigraphic matrix represents directed relationships between
#'  stratigraphic units. A stratigraphic matrix is an adjacency matrix (a non
#'  symmetric square matrix with zeros on its main diagonal), suitable to build
#'  a directed acyclic graph (DAG).
#' @seealso [as_stratigraphy()]
#' @example inst/examples/ex-stratigraphy.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases StratigraphicMatrix-class
.StratigraphicMatrix <- setClass(
  Class = "StratigraphicMatrix",
  contains = "LogicalMatrix"
)
