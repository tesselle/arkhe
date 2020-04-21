# CLASSES DEFINITION

# ======================================================================= Matrix
#' Matrix
#'
#' A virtual S4 class to represent a matrix. This is the mother class of all
#' matrix objects.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the matrix (UUID v4).
#' @slot size A length-two \code{\link{numeric}} vector giving the dimension
#'  of the matrix.
#' @slot row_names A \code{\link{character}} vector specifying the row names of
#'  the matrix.
#' @slot column_names A \code{\link{character}} vector specifying the column
#'  names of the matrix.
#' @slot dates A \code{\link{list}} giving the dates of each assemblage.
#' @slot coordinates A \code{\link{list}} giving the geographical coordinates of
#'  each assemblage (must considered as experimental and subject to major
#'  changes in a future release).
#' @section Matrix ID:
#'  When a matrix is first created, an identifier is generated (UUID v4).
#'  This ID is preserved when coercing to another class. Thus, the object ID is
#'  unique within the same class, but two objects of different classes can have
#'  the same ID. This makes it possible to identify objects representing the
#'  same initial data and associate them with the results of specific
#'  computations.
#' @section Get and set:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{get_id(x)}}{Get the ID of \code{x}.}
#'   \item{\code{get_dates(x)} and \code{set_dates(x) <- value}}{Get or set
#'   the dates of \code{x}.}
#'   \item{\code{get_coordinates(x)} and \code{set_coordinates(x) <- value}}{Get
#'   or set the geographical coordinates of \code{x}.}
#'  }
#' @section Access:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
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
#'   \item{\code{x[i, j]}}{Extracts elements selected by subscripts \code{i}
#'   and \code{j}. Indices are \code{\link{numeric}}, \code{\link{integer}} or
#'   \code{\link{character}} vectors or empty (missing) or \code{NULL}.
#'   Numeric values are coerced to \code{\link{integer}} as by
#'   \code{\link{as.integer}} (and hence truncated towards zero).
#'   Character vectors will be matched to the name of the elements.
#'   An empty index (a comma separated blank) indicates that all
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
setClass(
  Class = "GenericMatrix",
  slots = c(
    id = "character",
    size = "integer",
    row_names = "character",
    column_names = "character",
    dates = "list",
    coordinates = "list"
  ),
  contains = "VIRTUAL"
)

# --------------------------------------------------------------- Virtual matrix
#' Data Matrix
#'
#' A virtual S4 class to represent a data matrix.
#' @slot data An \code{\link{integer}}, a \code{\link{numeric}} or a
#'  \code{\link{logical}} vector (see details).
#' @param data A data vector.
#' @param nrow An \code{\link{integer}} value giving the desired number of rows.
#' @param ncol An \code{\link{integer}} value giving the desired number of
#'  columns.
# @param byrow A \code{\link{logical scalar}}: should the matrix be filled by
#  rows? If \code{FALSE} (the default) the matrix is filled by columns
#' @param dimnames A list of length 2 giving the row and column names
#'  respectively. If \code{NULL} (the default) dimension names will be created.
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @details
#'  \describe{
#'   \item{\code{IntegerMatrix}}{Values are coerced to \code{\link{integer}}
#'   as by \code{\link{as.integer}}.}
#'   \item{\code{NumericMatrix}}{Values are coerced to \code{\link{numeric}}
#'   as by \code{\link{as.numeric}}.}
#'   \item{\code{LogicalMatrix}}{Values are coerced to \code{\link{logical}}
#'   as by \code{\link{as.logical}}.}
#'  }
#' @seealso \linkS4class{GenericMatrix}
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @name DataMatrix-class
#' @rdname DataMatrix-class
#' @keywords internal
NULL

#' @aliases NumericMatrix-class
#' @rdname DataMatrix-class
setClass(
  Class = "IntegerMatrix",
  slot = c(
    data = "integer"
  ),
  contains = c("GenericMatrix", "VIRTUAL")
)

#' @aliases NumericMatrix-class
#' @rdname DataMatrix-class
setClass(
  Class = "NumericMatrix",
  slot = c(
    data = "numeric"
  ),
  contains = c("GenericMatrix", "VIRTUAL")
)

#' @aliases LogicalMatrix-class
#' @rdname DataMatrix-class
setClass(
  Class = "LogicalMatrix",
  slots = c(
    data = "logical"
  ),
  contains = c("GenericMatrix", "VIRTUAL")
)

setClassUnion(
  name = "DataMatrix",
  members = c("IntegerMatrix", "NumericMatrix", "LogicalMatrix")
)

# --------------------------------------------------------------- Integer matrix
#' Absolute Frequency Matrix
#'
#' An S4 class to represent an absolute frequency matrix (i.e. the number of
#' times a given datum occurs in a dataset).
#' @inheritParams DataMatrix-class
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .CountMatrix
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = "IntegerMatrix"
)

# --------------------------------------------------------------- Numeric matrix
#' Relative Frequency Matrix
#'
#' An S4 class to represent a relative frequency matrix (i.e. the fraction of
#' times a given datum occurs in a dataset).
#' @slot totals A \code{\link{numeric}} vector.
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .AbundanceMatrix
#' @aliases AbundanceMatrix-class
.AbundanceMatrix <- setClass(
  Class = "AbundanceMatrix",
  slots = c(
    totals = "numeric"
  ),
  contains = "NumericMatrix"
)

#' Co-Occurrence Matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main
#'  diagonal, which works out how many times (expressed in percent) each pairs
#'  of taxa/types occur together in at least one sample.
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .OccurrenceMatrix
#' @aliases OccurrenceMatrix-class
.OccurrenceMatrix <- setClass(
  Class = "OccurrenceMatrix",
  slots = c(
    n = "integer"
  ),
  contains = "NumericMatrix"
)

#' Similarity Matrix
#'
#' An S4 class to represent a (dis)similarity matrix.
#' @slot method A \code{\link{character}} string specifying the distance
#'  method used.
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @family matrix
#' @author N. Frerebeau
#' @docType class
#' @export .SimilarityMatrix
#' @aliases SimilarityMatrix-class
.SimilarityMatrix <- setClass(
  Class = "SimilarityMatrix",
  slots = c(
    method = "character"
  ),
  contains = "NumericMatrix"
)

# --------------------------------------------------------------- Logical matrix
#' Incidence Matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams DataMatrix-class
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \linkS4class{LogicalMatrix}
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .IncidenceMatrix
#' @aliases IncidenceMatrix-class
.IncidenceMatrix <- setClass(
  Class = "IncidenceMatrix",
  contains = "LogicalMatrix"
)

#' Stratigraphic Matrix
#'
#' An S4 class to represent a stratigraphic matrix.
#' @slot units A \code{\link{character}} vector giving the names of the
#'  stratigraphic unit.
#' @details
#'  A stratigraphic matrix represents directed relationships between
#'  stratigraphic units. A stratigraphic matrix is an adjacency matrix (a non
#'  symmetric square matrix with zeros on its main diagonal), suitable to build
#'  a directed acyclic graph (DAG).
#' @inheritSection GenericMatrix-class Matrix ID
#' @inheritSection GenericMatrix-class Get and set
#' @inheritSection GenericMatrix-class Access
#' @inheritSection GenericMatrix-class Subset
#' @seealso \linkS4class{LogicalMatrix}
#' @example inst/examples/ex-stratigraphy.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .StratigraphicMatrix
#' @aliases StratigraphicMatrix-class
.StratigraphicMatrix <- setClass(
  Class = "StratigraphicMatrix",
  contains = "LogicalMatrix"
)
