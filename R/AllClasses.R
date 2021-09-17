# CLASSES DEFINITION

# AbundanceMatrix ==============================================================
#' Archaeological Data
#'
#' A virtual S4 class to represent archaeological data.
#' @slot samples A [`character`] vector.
#' @slot groups A [`character`] vector.
#' @slot totals A [`numeric`] vector giving the absolute row sums.
#' @slot dates An [`integer`] vector specifying the date point estimate of each
#'  row.
#' @slot tpq An [`integer`] vector specifying the TPQ of each row.
#' @slot taq An [`integer`] vector specifying the TAQ of each row.
#' @section Get and set:
#'  In the code snippets below, `x` is an `AbundanceMatrix` object.
#'  \describe{
#'   \item{`get_samples(x)` and `get_samples(x) <- value`}{Get or set
#'   the sample names of `x`.}
#'   \item{`get_groups(x)` and `set_groups(x) <- value`}{Get or set
#'   the groups of `x`.}
#'   \item{`get_dates(x)` and `set_dates(x) <- value`}{Get or set
#'   the dates of `x`. `value` can be a [`list`] with components `tpq` (TPQ -
#'   *terminus post quem*) and `taq` (TAQ - *terminus ante quem*) or a
#'   [`numeric`] vector (point estimate). The elements of `value` are coerced to
#'   [`integer`] with [as.integer()] (hence truncated towards zero).}
#'  }
#' @section Abundance Matrix:
#'  The `CountMatrix`, `CompositionMatrix` and `IncidenceMatrix` classes have
#'  special slots:
#'
#'  * `samples` for replicated measurements/observation,
#'  * `groups` to group data by site/area,
#'  * `dates` to specify the date point estimate of an assemblage,
#'  * `tqp` and `taq` to specify the chronology of an assemblage.
#'
#'  When coercing a `data.frame` to a `*Matrix` object, an attempt is made to
#'  automatically assign values to these slots by mapping column names (case
#'  insensitive, plural insensitive). This behavior can be disabled by setting
#'  `options(arkhe.autodetect = FALSE)` or overrided by explicitly specifying
#'  the columns to be used in `as_*()`.
#' @section Chronology:
#'  The way chronological information is handled is somewhat opinionated.
#'  Sub-annual precision is overkill/meaningless in most situations: dates are
#'  assumed to be expressed in years CE and are stored as integers (values are
#'  coerced with `as.integer()` and hence truncated towards zero).
#' @author N. Frerebeau
#' @docType class
#' @aliases AbundanceMatrix-class
#' @keywords internal
.AbundanceMatrix <- setClass(
  Class = "AbundanceMatrix",
  slots = c(
    samples = "character",
    groups = "character",
    totals = "numeric",
    dates = "integer",
    tpq = "integer",
    taq = "integer"
  ),
  contains = "VIRTUAL"
)

.AbundanceSummary <- setClass(
  Class = "AbundanceSummary",
  slots = c(
    groups = "character",
    rows = "integer",
    columns = "integer",
    replicates = "logical",
    chronology = "matrix"
  )
)

# DataMatrix ===================================================================
#' Data Matrix
#'
#' S4 classes that represent a \eqn{m \times p}{m x p} matrix.
#' @note
#'  These classes inherit from [`matrix`].
#' @author N. Frerebeau
#' @family classes
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
#' @family classes
#' @docType class
#' @export .CountMatrix
#' @exportClass CountMatrix
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = c("IntegerMatrix", "AbundanceMatrix")
)

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
#' @family classes
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
#' @inheritParams base::matrix
#' @seealso [as_composition()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @export .CompositionMatrix
#' @exportClass CompositionMatrix
#' @aliases CompositionMatrix-class
.CompositionMatrix <- setClass(
  Class = "CompositionMatrix",
  contains = c("NumericMatrix", "AbundanceMatrix")
)

# LogicalMatrix ================================================================
## IncidenceMatrix -------------------------------------------------------------
#' Incidence Matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams base::matrix
#' @seealso [as_incidence()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @export .IncidenceMatrix
#' @exportClass IncidenceMatrix
#' @aliases IncidenceMatrix-class
.IncidenceMatrix <- setClass(
  Class = "IncidenceMatrix",
  contains = c("LogicalMatrix", "AbundanceMatrix")
)

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
#' @family classes
#' @docType class
#' @aliases StratigraphicMatrix-class
.StratigraphicMatrix <- setClass(
  Class = "StratigraphicMatrix",
  contains = "LogicalMatrix"
)
