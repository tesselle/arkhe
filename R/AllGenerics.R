# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("rownames")
setGeneric("colnames")
setGeneric("rownames<-")
setGeneric("colnames<-")
setGeneric("rowSums")
setGeneric("colSums")
setGeneric("rowMeans")
setGeneric("colMeans")
setGeneric("diag")
setGeneric("diag<-")
setGeneric("dist")
setGeneric("cov")
setGeneric("cor")
setGeneric("var")
setGeneric("sd")

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
#' @param value A possible value for the element(s) of \code{object}.
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

#' @rdname mutator
#' @aliases get_n-method
setGeneric(
  name = "get_n",
  def = function(x) standardGeneric("get_n")
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

# Operators ====================================================================
#' Common Operations on Matrix Objects
#'
#' Performs common operations on \code{DataMatrix} objects.
#' @param x,e1,e2 An object (typically a \linkS4class{DataMatrix} object).
#' @param digits A \code{\link{numeric}} value giving number of digits to be
#' used in \code{round} or \code{signif}.
#' @param ... Further arguments passed to or from methods.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  be removed?
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
#'   \item{\code{Math}}{"\code{abs}", "\code{sign}", "\code{sqrt}",
#'   "\code{ceiling}", "\code{floor}", "\code{trunc}", "\code{cummax}",
#'   "\code{cummin}", "\code{cumprod}", "\code{cumsum}", "\code{log}",
#'   "\code{log10}", "\code{log2}", "\code{log1p}", "\code{acos}",
#'   "\code{acosh}", "\code{asin}", "\code{asinh}", "\code{atan}",
#'   "\code{atanh}", "\code{exp}", "\code{expm1}", "\code{cos}", "\code{cosh}",
#'   "\code{cospi}", "\code{sin}", "\code{sinh}", "\code{sinpi}", "\code{tan}",
#'   "\code{tanh}", "\code{tanpi}", "\code{gamma}", "\code{lgamma}",
#'   "\code{digamma}", "\code{trigamma}"}
#'   \item{\code{Math2}}{"\code{round}", "\code{signif}"}
#'   \item{\code{Summary}}{"\code{min}", "\code{max}", "\code{range}",
#'   "\code{prod}", "\code{sum}", "\code{any}", "\code{all}"}
#'  }
#' @example inst/examples/ex-operators.R
#' @author N. Frerebeau
#' @docType methods
#' @family operator
#' @name operator
#' @rdname operator
NULL

# Statistics ===================================================================
#' Rowwise and Columnwise Statistics
#'
#' Computes statistics over \code{DataMatrix} margins.
#' @param x A \linkS4class{DataMatrix} object.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted from the calculations?
#' @details
#'  \describe{
#'   \item{\code{rowMeans} and \code{colMeans}}{Form row and column means.}
#'   \item{\code{rowSums} and \code{colSums}}{Form row and column sums.}
#'  }
#' @example inst/examples/ex-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name statistics
#' @rdname statistics
NULL

#' Correlation, Variance and Covariance
#'
#' Computes the variance, covariance or correlation of a \code{DataMatrix}.
#' @param x A \linkS4class{DataMatrix} object.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted from the calculations?
#' @param use An optional \code{\link{character}} string giving a method for
#'  computing covariances in the presence of missing values (see
#'  \code{\link[stats]{cor}}).
#' @param method A \code{\link{character}} string indicating which correlation
#'  coefficient (or covariance) is to be computed (see
#'  \code{\link[stats]{cor}}).
#' @example inst/examples/ex-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name correlation
#' @rdname correlation
NULL

# Summary ======================================================================
#' Matrix Summaries
#'
#' @param object A \linkS4class{DataMatrix} object.
#' @param ... Further arguments to be passed to \code{\link[base]{summary}}.
#' @return A \linkS4class{MatrixSummary} object.
#' @example inst/examples/ex-summary.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary
#' @name summary
#' @rdname summary
NULL

# Multivariate analysis ========================================================
#' Correspondence Analysis
#'
#' Computes a simple correspondence analysis based on the singular value
#' decomposition.
#' @param object A \linkS4class{CountMatrix} or a \linkS4class{CA} object.
#' @param n An \code{\link{integer}} value specifying the number of dimensions
#'  to be kept in the results (default to \code{5}).
#' @param sup_rows A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary rows.
#' @param sup_columns A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary columns.
#' @param data An object of supplementary points coercible to a
#'  \code{\link{matrix}} for which to compute principal coordinates.
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates rows (the default),
#'  \code{2} indicates columns.
#' @param standard A \code{\link{logical}} scalar: should standard coordinates
#'  be returned instead of principal coordinates (the default)?
#' @param sup A \code{\link{logical}} scalar: should supplementary points be
#'  returned?
#' @param sup_name A \code{\link{character}} string specifying the name of the
#'  column to create for supplementary points attribution (see below).
#' @param ... Currently not used.
#' @return
#'  \code{ca} returns a \linkS4class{CA} object.
#'
#'  \code{predict} returns a \code{data.frame} of principal coordinates.
#'
#'  \code{get_coordinates} returns a \code{data.frame} of coordinates. If
#'  \code{sup} is \code{TRUE}, an extra column (named after \code{sup_name}) is
#'  added specifying whether an observation is a supplementary point or not.
#'
#'  \code{get_inertia} returns a \code{data.frame}.
#'
#'  \code{get_distances} returns a \code{data.frame}.
#'
#'  \code{get_contributions} returns a \code{data.frame}.
#'
#'  \code{get_eigenvalues} returns a \code{data.frame} with the following
#'  columns: \code{eigenvalues}, \code{percentage} (percentage of variance) and
#'  \code{cumulative} (cumulative percentage of variance).
#' @example inst/examples/ex-ca.R
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name correspondence
#' @rdname correspondence
NULL

#' @rdname correspondence
#' @aliases ca-method
setGeneric(
  name = "ca",
  def = function(object, ...) standardGeneric("ca")
)

#' @rdname correspondence
#' @aliases get_coordinates-method
setGeneric(
  name = "get_coordinates",
  def = function(object, ...) standardGeneric("get_coordinates")
)

#' @rdname correspondence
#' @aliases get_contributions-method
setGeneric(
  name = "get_contributions",
  def = function(object, ...) standardGeneric("get_contributions")
)

#' @rdname correspondence
#' @aliases get_distances-method
setGeneric(
  name = "get_distances",
  def = function(object, ...) standardGeneric("get_distances")
)

#' @rdname correspondence
#' @aliases get_eigenvalues-method
setGeneric(
  name = "get_eigenvalues",
  def = function(object) standardGeneric("get_eigenvalues")
)

#' @rdname correspondence
#' @aliases get_inertia-method
setGeneric(
  name = "get_inertia",
  def = function(object, ...) standardGeneric("get_inertia")
)
