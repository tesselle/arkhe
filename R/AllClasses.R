# CLASSES DEFINITION
# https://stackoverflow.com/questions/11857658/assignment-of-s4-r-objects-to-a-matrix-why-does-this-work

# MultivariateAnalysis =========================================================
#' Correspondence Analysis Results
#'
#' An S4 class to store the results of a simple correspondence analysis.
#' @slot dimension An \code{\link{integer}} giving the dimension of the
#'  solution.
#' @slot row_names A \code{\link{character}} vector specifying the row names.
#' @slot row_coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the row standard coordinates.
#' @slot row_distances A \code{\link{numeric}} vector giving the row chi-square
#'  distances to centroid.
#' @slot row_inertias A \code{\link{numeric}} vector giving the row inertias.
#' @slot row_masses A \code{\link{numeric}} vector giving the row masses.
#' @slot row_supplement A \code{\link{logical}} vector specifying if a row is a
#'  supplementary point.
#' @slot column_names A \code{\link{character}} vector specifying the column
#'  names.
#' @slot column_coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the column standard coordinates.
#' @slot column_distances A \code{\link{numeric}} vector giving the column
#'  chi-square distances to centroid.
#' @slot column_inertias A \code{\link{numeric}} vector giving the column
#'  inertias.
#' @slot column_masses A \code{\link{numeric}} vector giving the column masses.
#' @slot column_supplement A \code{\link{logical}} vector specifying if a column
#'  is a supplementary point.
#' @slot singular_values A \code{\link{numeric}} vector giving the singular
#'  values.
#' @example inst/examples/ex-ca.R
#' @author N. Frerebeau
#' @family multivariate analysis
#' @docType class
#' @aliases CA-class
.CA <- setClass(
  Class = "CA",
  slots = c(
    dimension = "integer",
    row_names = "character",
    row_coordinates = "matrix",
    row_distances = "numeric",
    row_inertias = "numeric",
    row_masses = "numeric",
    row_supplement = "logical",
    column_names = "character",
    column_coordinates = "matrix",
    column_distances = "numeric",
    column_inertias = "numeric",
    column_masses = "numeric",
    column_supplement = "logical",
    singular_values = "numeric"
  )
)

# MatrixSummary ================================================================
#' Matrix Summary
#'
#' @example inst/examples/ex-summary.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases MatrixSummary-class
#' @keywords internal
.MatrixSummary <- setClass(
  Class = "MatrixSummary",
  contains = "list"
)

# DataMatrix ===================================================================
#' Data Matrix
#'
#' Virtual S4 classes that represent a \eqn{m \times p}{m x p} matrix.
#' @param data A data vector.
#' @param nrow An \code{\link{integer}} value giving the desired number of rows.
#' @param ncol An \code{\link{integer}} value giving the desired number of
#'  columns.
#' @param byrow A \code{\link{logical}} scalar: should the matrix be filled by
#'  rows? If \code{FALSE} (the default) the matrix is filled by columns.
#' @param dimnames A list of length 2 giving the row and column names
#'  respectively. If \code{NULL} (the default) dimension names will be created.
#' @slot size A length-two \code{\link{integer}} vector.
#' @slot row_names A length-\eqn{m} \code{\link{character}} vector.
#' @slot column_names A length-\eqn{p} \code{\link{character}} vector.
#' @slot group_names A length-\eqn{m} \code{\link{character}} vector.
#' @slot values An \code{\link{integer}}, a \code{\link{numeric}} or a
#'  \code{\link{logical}} vector (see details).
#' @details
#'  \code{DataMatrix} is the mother class of all matrix objects.
#'
#'  The \code{values} slot is defined in \code{IntegerMatrix},
#'  \code{NumericMatrix} and \code{LogicalMatrix} classes.
#' @section Get and set:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{get_groups(x)} and \code{set_groups(x) <- value}}{Get or set
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
#' @name DataMatrix-class
#' @rdname DataMatrix-class
#' @keywords internal
.DataMatrix <- setClass(
  Class = "DataMatrix",
  slots = c(
    size = "integer",
    row_names = "character",
    column_names = "character",
    group_names = "character"
  ),
  prototype = list(
    size = c(0L, 0L),
    row_names = character(0),
    column_names = character(0),
    group_names = character(0)
  ),
  contains = "VIRTUAL"
)

#' @aliases IntegerMatrix-class
#' @rdname DataMatrix-class
#' @export .IntegerMatrix
#' @exportClass IntegerMatrix
.IntegerMatrix <- setClass(
  Class = "IntegerMatrix",
  slots = c(
    values = "integer"
  ),
  contains = c("DataMatrix", "VIRTUAL")
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
  contains = c("DataMatrix", "VIRTUAL")
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
  contains = c("DataMatrix", "VIRTUAL")
)

# IntegerMatrix ================================================================
# CountMatrix ------------------------------------------------------------------
#' Absolute Frequency Matrix
#'
#' An S4 class to represent an absolute frequency matrix (i.e. the number of
#' times a given datum occurs in a dataset).
#' @inheritParams DataMatrix-class
#' @inheritSection DataMatrix-class Access
#' @inheritSection DataMatrix-class Subset
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

# OccurrenceMatrix -------------------------------------------------------------
#' Co-Occurrence Matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @slot n An \code{\link{integer}} giving the number of observations.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main
#'  diagonal, which works out how many times each pairs of taxa/types occur
#'  together in at least one sample.
#' @inheritSection DataMatrix-class Get and set
#' @inheritSection DataMatrix-class Access
#' @inheritSection DataMatrix-class Subset
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
#' @inheritSection DataMatrix-class Access
#' @inheritSection DataMatrix-class Subset
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

# SimilarityMatrix -------------------------------------------------------------
#' Similarity Matrix
#'
#' An S4 class to represent a (dis)similarity matrix.
#' @slot method A \code{\link{character}} string specifying the similarity
#'  method used.
#' @inheritSection DataMatrix-class Access
#' @inheritSection DataMatrix-class Subset
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
#' @inheritSection DataMatrix-class Access
#' @inheritSection DataMatrix-class Subset
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
#' @inheritSection DataMatrix-class Access
#' @inheritSection DataMatrix-class Subset
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
