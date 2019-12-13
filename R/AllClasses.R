# CLASSES DEFINITION

# ======================================================================= Matrix
#' Matrix
#'
#' An S4 class to represent a matrix. This class extends the \code{base}
#' \link[base]{matrix}.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the matrix (UUID v4).
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
#' @seealso \link[base]{matrix}
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases Matrix-class
#' @keywords internal
.Matrix <- setClass(
  Class = "Matrix",
  slots = c(
    id = "character",
    dates = "list",
    coordinates = "list"
  ),
  contains = "matrix"
)

# --------------------------------------------------------------- Numeric matrix
#' Numeric Matrix
#'
#' An S4 class to represent a numeric matrix.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{Matrix}
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases NumericMatrix-class
#' @keywords internal
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = "Matrix"
)

#' Absolute Frequency Matrix
#'
#' An S4 class to represent an absolute frequency matrix (i.e. the number of
#' times a given datum occurs in a dataset).
#' @inheritParams base::matrix
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @example inst/examples/ex-numeric-class.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .CountMatrix
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = "NumericMatrix"
)

#' Relative Frequency Matrix
#'
#' An S4 class to represent a relative frequency matrix (i.e. the fraction of
#' times a given datum occurs in a dataset).
#' @slot totals A \code{\link{numeric}} vector.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @example inst/examples/ex-numeric-class.R
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
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @example inst/examples/ex-numeric-class.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .OccurrenceMatrix
#' @aliases OccurrenceMatrix-class
.OccurrenceMatrix <- setClass(
  Class = "OccurrenceMatrix",
  contains = "NumericMatrix"
)

#' Similarity Matrix
#'
#' An S4 class to represent a (dis)similarity matrix.
#' @slot method A \code{\link{character}} string specifying the distance
#'  method used.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
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
#' Logical Matrix
#'
#' An S4 class to represent a logical matrix.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @note
#'  Numeric values are coerced to \code{\link{logical}} as by
#'  \code{\link{as.logical}}.
#' @seealso \linkS4class{Matrix}
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @aliases LogicalMatrix-class
#' @keywords internal
.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  contains = "Matrix"
)

#' Incidence Matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams base::matrix
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{LogicalMatrix}
#' @example inst/examples/ex-logical-class.R
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
#' @slot units A \code{\link{character}} vector giving the stratigraphic unit
#'  names.
#' @details
#'  A stratigraphic matrix represents directed relationships between
#'  stratigraphic units. A stratigraphic matrix is an adjacency matrix (a non
#'  symmetric square matrix with zeros on its main diagonal), suitable to build
#'  a directed acyclic graph (DAG).
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{LogicalMatrix}
#' @example inst/examples/ex-stratigraphy.R
#' @author N. Frerebeau
#' @family matrix
#' @docType class
#' @export .StratigraphicMatrix
#' @aliases StratigraphicMatrix-class
.StratigraphicMatrix <- setClass(
  Class = "StratigraphicMatrix",
  slots = c(
    units = "character"
  ),
  contains = "LogicalMatrix"
)
