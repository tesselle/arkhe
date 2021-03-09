# GENERIC METHODS
#' @include AllClasses.R
NULL

# Clean ========================================================================
#' Data Replacement
#'
#' Removes empty row/column or row/column with missing values or zeros.
#' @param x A [`matrix`], a [`data.frame`] or a `*Matrix` object.
#' @param value A possible value to replace missing values of `x`.
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family utilities
#' @name replace
#' @rdname replace
NULL

#' @rdname replace
#' @aliases replace_NA-method
setGeneric(
  name = "replace_NA",
  def = function(x, ...) standardGeneric("replace_NA")
)

#' Data Cleaning
#'
#' Removes empty row/column or row/column with missing values or zeros.
#' @param x A [`matrix`], a [`data.frame`] or a `*Matrix` object.
#' @param margin An [`integer`] giving the subscript which the
#'  cleaning will be applied over (`1` indicates rows, `2` indicates
#'  columns).
#' @param finite A [`logical`] scalar: should non-[`finite`] values also be
#'  removed?
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family utilities
#' @name remove
#' @rdname remove
NULL

#' @rdname remove
#' @aliases remove_NA-method
setGeneric(
  name = "remove_NA",
  def = function(x, ...) standardGeneric("remove_NA")
)

#' @rdname remove
#' @aliases remove_empty-method
setGeneric(
  name = "remove_empty",
  def = function(x, ...) standardGeneric("remove_empty")
)

#' @rdname remove
#' @aliases remove_zero-method
setGeneric(
  name = "remove_zero",
  def = function(x, ...) standardGeneric("remove_zero")
)

# Coerce =======================================================================
#' Coerce
#'
#' @param from An object to be coerced.
#' @param factor A [`logical`] scalar: should character string be
#'  coerced to [`factor`]? Default to `FALSE`, if `TRUE` the original ordering is
#'  preserved.
#' @param reverse A [`logical`] scalar: should the order of factor levels be
#'  reversed? Only used if `factor` is `TRUE`. Useful for plotting.
#' @param ... Currently not used.
#' @details
#'  The following methods coerce an object to a `*Matrix` object:
#'
#'  \tabular{lll}{
#'   **Method** \tab **Target** \tab **Details** \cr
#'   `as_count()` \tab [CountMatrix-class] \tab absolute frequency data \cr
#'   `as_composition()` \tab [CompositionMatrix-class] \tab relative frequency data \cr
#'   `as_incidence()` \tab [IncidenceMatrix-class] \tab presence/absence data \cr
#'   `as_occurrence()` \tab [OccurrenceMatrix-class] \tab co-occurrence \cr
#'   `as_stratigraphy()` \tab [StratigraphicMatrix-class] \tab stratigraphic relationships
#'  }
#'
#'  **Note that `as_count` rounds numeric values to zero decimal places and
#'  then coerces to integer as by `as.integer()`.**
#'
#'  `as_stratigraphy()` converts a set of stratigraphic relationships (edges)
#'  to a stratigraphic (adjacency) matrix. `from` can be a [`matrix`], [`list`],
#'  or [`data.frame`]: the first column/component is assumed to contain the
#'  bottom units and the second the top units (adjacency).
#'
#'  \tabular{lll}{
#'   **Method** \tab **Target** \tab **Details** \cr
#'   `as_long()` \tab [`data.frame`] \tab long S3 data frame \cr
#'   `as_features()` \tab [`data.frame`] \tab wide S3 data frame \cr
#'  }
#'
#'  `as_features()` converts a `*Matrix` object to a collection of features:
#'  a [`data.frame`] with all informations as extra columns (result may differ
#'  according to the class of `from`).
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
  def = function(from, ...) standardGeneric("as_long"),
  valueClass = "data.frame"
)

#' @rdname coerce
#' @aliases as_count-method
setGeneric(
  name = "as_count",
  def = function(from) standardGeneric("as_count"),
  valueClass = "CountMatrix"
)

#' @rdname coerce
#' @aliases as_composition-method
setGeneric(
  name = "as_composition",
  def = function(from) standardGeneric("as_composition"),
  valueClass = "CompositionMatrix"
)

#' @rdname coerce
#' @aliases as_abundance-method
setGeneric(
  name = "as_abundance",
  def = function(from) standardGeneric("as_abundance"),
  valueClass = "CompositionMatrix"
)

#' @rdname coerce
#' @aliases as_incidence-method
setGeneric(
  name = "as_incidence",
  def = function(from) standardGeneric("as_incidence"),
  valueClass = "IncidenceMatrix"
)

#' @rdname coerce
#' @aliases as_occurrence-method
setGeneric(
  name = "as_occurrence",
  def = function(from) standardGeneric("as_occurrence"),
  valueClass = "OccurrenceMatrix"
)

#' @rdname coerce
#' @aliases as_features-method
setGeneric(
  name = "as_features",
  def = function(from) standardGeneric("as_features"),
  valueClass = "data.frame"
)

#' @rdname coerce
#' @aliases as_stratigraphy-method
setGeneric(
  name = "as_stratigraphy",
  def = function(from) standardGeneric("as_stratigraphy"),
  valueClass = "StratigraphicMatrix"
)

# Extract ======================================================================
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s) (typically a
#'  `*Matrix` object).
#' @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases has_groups-method
setGeneric(
  name = "has_groups",
  def = function(x) standardGeneric("has_groups")
)

#' @rdname mutator
#' @aliases get_groups-method
setGeneric(
  name = "get_groups",
  def = function(x) standardGeneric("get_groups")
)

#' @rdname mutator
#' @aliases set_groups-method
setGeneric(
  name = "set_groups<-",
  def = function(x, value) standardGeneric("set_groups<-")
)

#' @rdname mutator
#' @aliases get_samples-method
setGeneric(
  name = "get_samples",
  def = function(x) standardGeneric("get_samples")
)

#' @rdname mutator
#' @aliases set_samples-method
setGeneric(
  name = "set_samples<-",
  def = function(x, value) standardGeneric("set_samples<-")
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

# Subset =======================================================================
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s) (typically a `*Matrix` object).
#' @param i,j Indices specifying elements to extract or replace. Indices are
#'  [`numeric`], [`integer`] or [`character`] vectors or empty (missing) or
#'  `NULL`. Numeric values are coerced to [`integer`] as by [as.integer()]
#'  (and hence truncated towards zero). Character vectors will be matched to
#'  the name of the elements. An empty index (a comma separated blank) indicates
#'  that all entries in that dimension are selected.
#' @param value A possible value for the element(s) of `x`.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
#' @param ... Currently not used.
#' @return
#'  A subsetted object of the same sort as `x`.
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL
