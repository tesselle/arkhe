# GENERIC METHODS
#' @include AllClasses.R
NULL

# Add S4 dispatch to base S3 generic
setGeneric("length")
setGeneric("diag")
setGeneric("row")
setGeneric("col")
setGeneric("nrow")
setGeneric("ncol")
setGeneric("rownames")
setGeneric("rownames<-")
setGeneric("colnames")
setGeneric("colnames<-")
setGeneric("rowSums")
setGeneric("colSums")
setGeneric("rowMeans")
setGeneric("colMeans")

# ======================================================================= Coerce
#' Coerce
#'
#' @param from An object to be coerced.
#' @param as_factor A \code{\link{logical}} scalar: should character string be
#'  coerced to \code{\link{factor}}? Default to \code{FALSE}, if \code{TRUE}
#'  the original ordering is preserved.
#' @param ... Currently not used.
#' @details
#'  The following methods coerce an object to a \code{Matrix} object:
#'
#'  \tabular{lll}{
#'   \strong{Method} \tab \strong{Target} \tab \strong{Details} \cr
#'   \code{as_count} \tab \linkS4class{CountMatrix} \tab absolute frequency data \cr
#'   \code{as_abundance} \tab \linkS4class{AbundanceMatrix} \tab relative frequency data \cr
#'   \code{as_incidence} \tab \linkS4class{IncidenceMatrix} \tab presence/absence data \cr
#'   \code{as_occurrence} \tab \linkS4class{OccurrenceMatrix} \tab co-occurrence \cr
#'   \code{as_similarity} \tab \linkS4class{SimilarityMatrix} \tab (dis)similarity \cr
#'   \code{as_stratigraphy} \tab \linkS4class{StratigraphicMatrix} \tab stratigraphic relationships
#'  }
#'
#'  **Note that \code{as_count} rounds numeric values to zero decimal places and
#'  then coerces to integer as by \code{\link{as.integer}}.**
#'
#'  \tabular{lll}{
#'   \strong{Method} \tab \strong{Target} \tab \strong{Details} \cr
#'   \code{as.matrix} \tab \code{\link{matrix}} \tab S3 matrix \cr
#'   \code{as.data.frame} \tab \code{\link{data.frame}} \tab S3 data frame \cr
#'   \code{as_long} \tab \code{\link{data.frame}} \tab long S3 data frame \cr
#'  }
#'
#'  \code{as_stratigraphy} converts a set of stratigraphic relationships (edges)
#'  to a stratigraphic (adjacency) matrix. \code{from} can be a
#'  \code{\link{matrix}}, \code{\link{list}}, or \code{\link{data.frame}}:
#'  the first column/component is assumed to contain the bottom units and the
#'  second the top units.
#'
#'  \code{as_features} converts an \linkS4class{Matrix} object to a
#'  collection of features (i.e. a\code{\link{data.frame}} with
#'  dates and coordinates columns).
#' @return A coerced object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family matrix
#' @name coerce
#' @rdname coerce
NULL

#' @rdname coerce
#' @aliases as_long-method
setGeneric(
  name = "as_long",
  def = function(from, ...) standardGeneric("as_long")
)

#' @rdname coerce
#' @aliases as_count-method
setGeneric(
  name = "as_count",
  def = function(from) standardGeneric("as_count")
)

#' @rdname coerce
#' @aliases as_abundance-method
setGeneric(
  name = "as_abundance",
  def = function(from) standardGeneric("as_abundance")
)

#' @rdname coerce
#' @aliases as_incidence-method
setGeneric(
  name = "as_incidence",
  def = function(from) standardGeneric("as_incidence")
)

#' @rdname coerce
#' @aliases as_occurrence-method
setGeneric(
  name = "as_occurrence",
  def = function(from) standardGeneric("as_occurrence")
)

#' @rdname coerce
#' @aliases as_similarity-method
setGeneric(
  name = "as_similarity",
  def = function(from) standardGeneric("as_similarity")
)

#' @rdname coerce
#' @aliases as_features-method
setGeneric(
  name = "as_features",
  def = function(from) standardGeneric("as_features")
)

#' @rdname coerce
#' @aliases as_stratigraphy-method
setGeneric(
  name = "as_stratigraphy",
  def = function(from) standardGeneric("as_stratigraphy")
)

# ====================================================================== Extract
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s).
#' @param as.factor A \code{\link{logical}} scalar: should the value be
#'  returned as a factor of row or column labels rather than as numbers?
# @param names A \code{\link{logical}} scalar: should the resulting vector
#  inherit names?
#' @param value A possible value for the element(s) of \code{object} (see
#'  below).
#' @return
#'  An object of the same sort as \code{x} with the new values assigned.
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_id-method
setGeneric(
  name = "get_id",
  def = function(x) standardGeneric("get_id")
)

#' @rdname mutator
#' @aliases set_id-method
setGeneric(
  name = "set_id<-",
  def = function(x, value) standardGeneric("set_id<-")
)

#' @rdname mutator
#' @aliases get_method-method
setGeneric(
  name = "get_method",
  def = function(x) standardGeneric("get_method")
)

#' @rdname mutator
#' @aliases set_method-method
setGeneric(
  name = "set_method<-",
  def = function(x, value) standardGeneric("set_method<-")
)

#' @rdname mutator
#' @aliases get_units-method
setGeneric(
  name = "get_units",
  def = function(x) standardGeneric("get_units")
)

#' @rdname mutator
#' @aliases get_totals-method
setGeneric(
  name = "get_totals",
  def = function(x) standardGeneric("get_totals")
)

#' @rdname mutator
#' @aliases set_totals-method
setGeneric(
  name = "set_totals<-",
  def = function(x, value) standardGeneric("set_totals<-")
)

#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j Indices specifying elements to extract or replace. Indices are
#'  \code{\link{numeric}}, \code{\link{integer}} or \code{\link{character}}
#'  vectors or empty (missing) or \code{NULL}. Numeric values are coerced to
#'  \code{\link{integer}} as by \code{\link{as.integer}} (and hence truncated
#'  towards zero). Character vectors will be matched to the name of the
#'  elements. An empty index (a comma separated blank) indicates that all
#'  entries in that dimension are selected.
#' @param value A possible value for the element(s) of \code{x}.
#' @param drop A \code{\link{logical}} scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
#' @param exact A \code{\link{logical}} scalar: should partial matching be used?
#'  If \code{TRUE} (the default), no partial matching is allowed.
#' @param ... Currently not used.
#' @return
#'  A subsetted object of the same sort as \code{x}.
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# ==================================================================== Operators
#' Common Operations on Matrix Objects
#'
#' Performs common operations on \code{DataMatrix} objects.
#' @param x,e1,e2 An object (typically a \linkS4class{DataMatrix} object).
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted from the calculations?
#' @section Group Generics:
#'  \linkS4class{DataMatrix} objects have support for S4 group generic
#'  functionality to operate within elements across objects:
#'  \describe{
#'   \item{\code{Arith}}{"\code{+}", "\code{-}", "\code{*}", "\code{^}",
#'   "\code{\%\%}", "\code{\%/\%}", "\code{/}"}
#'   \item{\code{Compare}}{"\code{==}", "\code{>}", "\code{<}", "\code{!=}",
#'   "\code{<=}", "\code{>=}"}
#'   \item{\code{Logic}}{"\code{&}", "\code{|}"}
#'   \item{\code{Ops}}{"\code{Arith}", "\code{Compare}", "\code{Logic}"}
#'   \item{\code{Summary}}{"\code{min}", "\code{max}", "\code{range}",
#'   "\code{prod}", "\code{sum}", "\code{any}", "\code{all}"}
#'  }
#' @section Other Methods:
#'  \describe{
#'   \item{\code{rowAll} and \code{colAll}}{Check if all values are
#'   \code{TRUE} according to a given predicate.}
#'   \item{\code{rowAny} and \code{colAny}}{Check if some values are
#'   \code{TRUE} according to a given predicate.}
#'   \item{\code{rowCount} and \code{colCount}}{Count values in rows or columns
#'   according to a given predicate.}
#'  }
#' @example inst/examples/ex-operators.R
#' @author N. Frerebeau
#' @docType methods
#' @family operator
#' @name operator
#' @rdname operator
NULL

# =================================================================== Statistics
#' Rowwise and Columnwise Statistics
#'
#' Computes statistics over \code{DataMatrix} margins.
#' @param x A \linkS4class{DataMatrix} object.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted from the calculations?
#' @details
#'  \describe{
#'   \item{\code{rowQuantiles} and \code{colQuantiles}}{Form row and column
#'   quantiles.}
#'   \item{\code{rowMeans} and \code{colMeans}}{Form row and column means.}
#'   \item{\code{rowRanges} and \code{colRanges}}{Form row and column ranges.}
#'   \item{\code{rowRanks} and \code{colRanks}}{}
#'   \item{\code{rowSums} and \code{colSums}}{Form row and column sums.}
#'   \item{\code{rowVars} and \code{colVars}}{Form row and column variances.}
#'  }
#' @example inst/examples/ex-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name statistics
#' @rdname statistics
NULL

# =================================================================== Chronology
#' Chronological Information
#'
#' \code{set_dates} and \code{get_dates} allow to get or set the dates of an
#' object.
#' @param object A \eqn{m \times p}{m x p} matrix of count data (typically of
#'  class \linkS4class{Matrix}).
#' @param value A possible value for the element(s) of \code{object} (see
#'  below).
#' @details
#'  An attempt is made to interpret the argument \code{value} in a suitable way.
#'  \emph{Note} that errors are assumed to be given at \code{1} sigma.
#'
#'  If \code{value} is a:
#'  \describe{
#'   \item{\code{character} vector}{it is assumed to contain Roman numerals.}
#'   \item{\code{numeric} or \code{integer} \code{vector}}{these values are
#'   assumed to represent dates without known errors.}
#'   \item{\code{list}}{containing components "\code{value}" and "\code{error}",
#'   these are used to define dates and corresponding errors.}
#'   \item{\code{matrix} or \code{data.frame} with two or more columns}{the
#'   first is assumed to contain the dates and the second the error values.
#'   \emph{Note} that if \code{value} has columns named "\code{value}" and
#'   "\code{error}", these columns will be used.}
#'  }
#' @return
#'  \code{get_dates} returns a two-columns \code{\link{data.frame}}.
#' @example inst/examples/ex-chronology.R
#' @author N. Frerebeau
#' @family chronology
#' @docType methods
#' @name chronology
#' @rdname chronology
NULL

#' @rdname chronology
#' @aliases get_dates-method
setGeneric(
  name = "get_dates",
  def = function(object) standardGeneric("get_dates")
)

#' @rdname chronology
#' @aliases set_dates-method
setGeneric(
  name = "set_dates<-",
  def = function(object, value) standardGeneric("set_dates<-")
)

# ==================================================================== Geography
#' Spatial Information
#'
#' Deal with spatial information.
#' @param object An object from which to get or set element(s).
#' @param value A possible value for the element(s) of \code{object} (see
#'  below).
#' @details
#'  An attempt is made to interpret the argument \code{value} in a way suitable
#'  for geographic coordinates.
#'
#'  If \code{value} is a:
#'  \describe{
#'   \item{\code{list}}{containing components "\code{x}", "\code{y}" and
#'   "\code{z}", these are used to define coordinates (longitude, latitude and
#'   elevation, respectively). If "\code{z}" is missing, the vertical
#'   coordinates will be ignored (and \code{NA} will be generated).}
#'   \item{\code{matrix} or \code{data.frame} with two or more columns}{the
#'   first is assumed to contain the \code{x} values, the second the \code{y}
#'   and the third the \code{z} values. \emph{Note} that if \code{value} has
#'   columns named "\code{x}", "\code{y}" and "\code{z}", these columns will be
#'   used. If \code{value} has only two columns or has columns named "\code{x}"
#'   and "\code{y}" but not "\code{z}", the vertical coordinates will be ignored
#'   (and \code{NA} will be generated).}
#'  }
#' @note EXPERIMENTAL: subject to major changes in a future release.
#' @example inst/examples/ex-geography.R
#' @author N. Frerebeau
#' @family geography
#' @docType methods
#' @name geography
#' @rdname geography
NULL

#' @rdname geography
#' @aliases get_coordinates-method
setGeneric(
  name = "get_coordinates",
  def = function(object) standardGeneric("get_coordinates")
)

#' @rdname geography
#' @aliases set_coordinates-method
setGeneric(
  name = "set_coordinates<-",
  def = function(object, value) standardGeneric("set_coordinates<-")
)
