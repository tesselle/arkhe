# GENERIC METHODS
#' @include AllClasses.R
NULL

# ====================================================================== Extract
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param object An object from which to get or set element(s).
#' @param value A possible value for the element(s) of \code{object} (see
#'  below).
#' @return
#'  An object of the same sort as \code{object} with the new values assigned.
#' @example inst/examples/ex-abundance-class.R
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
  def = function(object) standardGeneric("get_id")
)

#' @rdname mutator
#' @aliases get_totals-method
setGeneric(
  name = "get_totals",
  def = function(object) standardGeneric("get_totals")
)

#' @rdname mutator
#' @aliases set_totals-method
setGeneric(
  name = "set_totals<-",
  def = function(object, value) standardGeneric("set_totals<-")
)

# ------------------------------------------------------------------------------
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
# @param drop A \code{\link{logical}} scalar: should the result be coerced to
#  the lowest possible dimension? This only works for extracting elements,
#  not for the replacement.
#' @return
#'  A subsetted object.
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# ------------------------------------------------------------------------------
#' Coerce
#'
#' @param from A numeric \code{\link{matrix}} or \code{\link{data.frame}} to be
#'  coerced.
#' @details
#'  The following methods coerce a \code{matrix} or \code{data.frame} to a
#'  \code{*Matrix} object:
#'
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Target} \cr
#'   \code{as_count} \tab \linkS4class{CountMatrix} \cr
#'   \code{as_frequency} \tab \linkS4class{RelativeFrequencyMatrix} \cr
#'   \code{as_incidence} \tab \linkS4class{IncidenceMatrix} \cr
#'   \code{as_occurrence} \tab \linkS4class{OccurrenceMatrix} \cr
#'   \code{as_similarity} \tab \linkS4class{SimilarityMatrix} \cr
#'   \code{as_stratigraphy} \tab \linkS4class{StratigraphicMatrix}
#'  }
#'
#'  \code{as_features} converts an \code{AbundanceMatrix} object to a
#'  collection of features (i.e. a\code{\link{data.frame}} with
#'  dates and coordinates columns) that can be used for spatial manipulation
#'  with \pkg{sf}.
#'
#'  \code{as_stratigraphy} converts a set of stratigraphic relationships (edges)
#'  to a stratigraphic (adjacency) matrix. \code{from} can be a
#'  \code{\link{matrix}}, \code{\link{list}}, or \code{\link{data.frame}}:
#'  the first column/component is assumed to contain the bottom units and the
#'  second the top units.
#' @return A coerced object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family matrix
#' @name coerce
#' @rdname coerce
NULL

#' @rdname coerce
#' @aliases as_count-method
setGeneric(
  name = "as_count",
  def = function(from) standardGeneric("as_count")
)

#' @rdname coerce
#' @aliases as_frequency-method
setGeneric(
  name = "as_frequency",
  def = function(from) standardGeneric("as_frequency")
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

# =================================================================== Chronology
#' Chronological Information
#'
#' \code{set_dates} and \code{get_dates} allow to get or set the dates of an
#' object.
#' @param object A \eqn{m \times p}{m x p} matrix of count data (typically of
#'  class \linkS4class{CountMatrix}).
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
#' @name time
#' @rdname time
NULL

#' @rdname time
#' @aliases get_dates-method
setGeneric(
  name = "get_dates",
  def = function(object) standardGeneric("get_dates")
)

#' @rdname time
#' @aliases set_dates-method
setGeneric(
  name = "set_dates<-",
  def = function(object, value) standardGeneric("set_dates<-")
)
