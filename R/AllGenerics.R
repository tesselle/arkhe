# GENERIC METHODS
#' @include AllClasses.R
NULL

# Clean ========================================================================
#' Data Cleaning
#'
#' Removes empty row/column or row/column with missing values or zeros.
#' @param x A \code{\link{matrix}}, \code{\link{data.frame}} or
#'  \linkS4class{DataMatrix} object.
#' @param margin An \code{\link{integer}} giving the subscript which the
#'  cleaning will be applied over (\code{1} indicates rows, \code{2} indicates
#'  columns).
#' @param finite A \code{\link{logical}} scalar: should non-\code{\link{finite}}
#'  values also be removed?
#' @param value A possible value to replace missing values of \code{x}.
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family utilities
#' @name clean
#' @rdname clean
NULL

#' @rdname clean
#' @aliases replace_NA-method
setGeneric(
  name = "replace_NA",
  def = function(x, ...) standardGeneric("replace_NA")
)

#' @rdname clean
#' @aliases remove_NA-method
setGeneric(
  name = "remove_NA",
  def = function(x, ...) standardGeneric("remove_NA")
)

#' @rdname clean
#' @aliases remove_empty-method
setGeneric(
  name = "remove_empty",
  def = function(x, ...) standardGeneric("remove_empty")
)

#' @rdname clean
#' @aliases remove_zero-method
setGeneric(
  name = "remove_zero",
  def = function(x, ...) standardGeneric("remove_zero")
)

# Coerce =======================================================================
#' Coerce
#'
#' @param from An object to be coerced.
#' @param factor A \code{\link{logical}} scalar: should character string be
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
#'  second the top units (adjacency).
#'
#'  \code{as_features} converts an \linkS4class{DataMatrix} object to a
#'  collection of features: a \code{\link{data.frame}} with all informations
#'  as extra columns (result may differ according to the class of \code{from}).
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

# Extract ======================================================================
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s) (typically a
#'  \linkS4class{DataMatrix} object).
#' @param value A possible value for the element(s) of \code{x}.
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
#' @aliases get_groups-method
setGeneric(
  name = "get_groups",
  def = function(x) standardGeneric("get_groups")
)

#' @rdname mutator
#' @aliases has_groups-method
setGeneric(
  name = "has_groups",
  def = function(x) standardGeneric("has_groups")
)

#' @rdname mutator
#' @aliases set_groups-method
setGeneric(
  name = "set_groups<-",
  def = function(x, value) standardGeneric("set_groups<-")
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

# Subset =======================================================================
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
