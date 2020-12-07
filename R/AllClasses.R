# CLASSES DEFINITION
# https://stackoverflow.com/questions/11857658/assignment-of-s4-r-objects-to-a-matrix-why-does-this-work

# GenericMatrix ================================================================
#' Matrix
#'
#' An S4 class to represent a matrix. This is the mother class of all
#' matrix objects.
#' @slot size An \code{\link{integer}} vector.
#' @slot row_names A \code{\link{character}} vector.
#' @slot column_names A \code{\link{character}} vector.
#' @slot sample_names A \code{\link{factor}} vector.
#' @section Get and set:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{get_samples(x)} and \code{set_samples(x) <- value}}{Get or set
#'   the sample names of \code{x}.}
#'  }
#' @section Access:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{length(x)}}{Returns the length of \code{x}.}
#'   \item{\code{dim(x)}}{Returns the dimension of \code{x}.}
#'   \item{\code{nrow(x)}}{Returns the number of rows present in \code{x}.}
#'   \item{\code{ncol(x)}}{Returns the number of columns present in \code{x}.}
#'   \item{\code{dimnames(x)}, \code{dimnames(x) <- value}}{Retrieves or sets
#'   the row dimnames of \code{x} according to \code{value}.}
#'   \item{\code{rownames(x)}, \code{rownames(x) <- value}}{Retrieves or sets
#'   the row names of \code{x} according to \code{value}.}
#'   \item{\code{colnames(x)}, \code{colnames(x) <- value}}{Retrieves or sets
#'   the column names of \code{x} according to \code{value}.}
#'  }
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{x[i, j, ..., drop]}}{Extracts elements selected by subscripts
#'   \code{i} and \code{j}. Indices are \code{\link{numeric}} or
#'   \code{\link{character}} vectors or empty (missing) or \code{NULL}.
#'   Numeric values are coerced to \code{\link{integer}} as by
#'   \code{\link{as.integer}} (and hence truncated towards zero).
#'   Character vectors will be matched to the \code{\link{dimnames}} of the
#'   object. An empty index (a comma separated blank) indicates that all
#'   entries in that dimension are selected.
#'   Returns an object of the same class as \code{x}.}
#'   \item{\code{x[[i]]}}{Extracts a single element selected by subscript
#'   \code{i}.}
#'  }
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases GenericMatrix-class
#' @keywords internal
.GenericMatrix <- setClass(
  Class = "GenericMatrix",
  slots = c(
    size = "integer",
    row_names = "character",
    column_names = "character",
    sample_names = "factor"
  ),
  prototype = list(
    size = c(0L, 0L),
    row_names = character(0),
    column_names = character(0),
    sample_names = factor()
  ),
  contains = "VIRTUAL"
)

# DataMatrix ===================================================================
#' Data Matrix
#'
#' A virtual S4 class to represent a data matrix.
#' @slot values An \code{\link{integer}}, a \code{\link{numeric}} or a
#'  \code{\link{logical}} vector (see details).
#' @param data A data vector.
#' @param nrow An \code{\link{integer}} value giving the desired number of rows.
#' @param ncol An \code{\link{integer}} value giving the desired number of
#'  columns.
#' @param byrow A \code{\link{logical}} scalar: should the matrix be filled by
#'  rows? If \code{FALSE} (the default) the matrix is filled by columns.
#' @param dimnames A list of length 2 giving the row and column names
#'  respectively. If \code{NULL} (the default) dimension names will be created.
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @name DataMatrix-class
#' @rdname DataMatrix-class
#' @keywords internal
NULL

#' @aliases IntegerMatrix-class
#' @rdname DataMatrix-class
#' @export .IntegerMatrix
#' @exportClass IntegerMatrix
.IntegerMatrix <- setClass(
  Class = "IntegerMatrix",
  slots = c(
    values = "integer"
  ),
  contains = c("GenericMatrix", "VIRTUAL")
)

#' @aliases NumericMatrix-class
#' @rdname DataMatrix-class
#' @export .NumericMatrix
#' @exportClass NumericMatrix
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  slots = c(
    values = "numeric"
  ),
  contains = c("GenericMatrix", "VIRTUAL")
)

#' @aliases LogicalMatrix-class
#' @rdname DataMatrix-class
#' @export .LogicalMatrix
#' @exportClass LogicalMatrix
.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  slots = c(
    values = "logical"
  ),
  contains = c("GenericMatrix", "VIRTUAL")
)

setClassUnion(
  name = "DataMatrix",
  members = c("IntegerMatrix", "NumericMatrix", "LogicalMatrix")
)

# IntegerMatrix ================================================================
# CountMatrix ------------------------------------------------------------------
#' Absolute Frequency Matrix
#'
#' An S4 class to represent an absolute frequency matrix (i.e. the number of
#' times a given datum occurs in a dataset).
#' @inheritParams DataMatrix-class
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \link{as_count}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .CountMatrix
#' @exportClass CountMatrix
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = "IntegerMatrix"
)

# NumericMatrix ================================================================
# AbundanceMatrix --------------------------------------------------------------
#' Relative Frequency Matrix
#'
#' An S4 class to represent a relative frequency matrix (i.e. the fraction of
#' times a given datum occurs in a dataset).
#' @slot totals A \code{\link{numeric}} vector giving the absolute row sums.
#' @inheritParams DataMatrix-class
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
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
  contains = "NumericMatrix"
)

# OccurrenceMatrix -------------------------------------------------------------
#' Co-Occurrence Matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @slot n TODO.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main
#'  diagonal, which works out how many times (expressed in percent) each pairs
#'  of taxa/types occur together in at least one sample.
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \link{as_occurrence}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases OccurrenceMatrix-class
.OccurrenceMatrix <- setClass(
  Class = "OccurrenceMatrix",
  slots = c(
    n = "integer"
  ),
  contains = "NumericMatrix"
)

# SimilarityMatrix -------------------------------------------------------------
#' Similarity Matrix
#'
#' An S4 class to represent a (dis)similarity matrix.
#' @slot method A \code{\link{character}} string specifying the similarity
#'  method used.
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \link{as_similarity}
#' @family matrix
#' @author N. Frerebeau
#' @docType class
#' @aliases SimilarityMatrix-class
.SimilarityMatrix <- setClass(
  Class = "SimilarityMatrix",
  slots = c(
    method = "character"
  ),
  contains = "NumericMatrix"
)

# LogicalMatrix ================================================================
# IncidenceMatrix --------------------------------------------------------------
#' Incidence Matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams DataMatrix-class
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
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
  contains = "LogicalMatrix"
)

# Stratigraphic ----------------------------------------------------------------
#' Stratigraphic Matrix
#'
#' An S4 class to represent a stratigraphic matrix.
#' @details
#'  A stratigraphic matrix represents directed relationships between
#'  stratigraphic units. A stratigraphic matrix is an adjacency matrix (a non
#'  symmetric square matrix with zeros on its main diagonal), suitable to build
#'  a directed acyclic graph (DAG).
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
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
