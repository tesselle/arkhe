# GENERIC METHODS
#' @include AllClasses.R
NULL

# Data Cleaning ================================================================
## Assign ----------------------------------------------------------------------
#' Assign a specific row/column to the column/row names
#'
#'
#' @param x A [`data.frame`].
#' @param row A length-one [`numeric`] vector specifying the row number that is
#'  to become the column names.
#' @param column A length-one [`numeric`] vector specifying the column number
#'  that is to become the row names.
#' @param remove A [`logical`] scalar: should the specified row/column be removed
#'  after making it the column/row names?
#' @param ... Currently not used.
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name assign
#' @rdname assign
NULL

#' @rdname assign
#' @aliases assign_colnames-method
setGeneric(
  name = "assign_colnames",
  def = function(x, ...) standardGeneric("assign_colnames")
)

#' @rdname assign
#' @aliases assign_rownames-method
setGeneric(
  name = "assign_rownames",
  def = function(x, ...) standardGeneric("assign_rownames")
)

## Count -----------------------------------------------------------------------
#' Count values according to a given predicate
#'
#' Counts values by rows/columns according to a given predicate.
#' @param x An object (should be a [`matrix`] or a [`data.frame`]).
#' @param f A predicate [`function`].
#' @param margin A vector giving the subscripts which the function will be
#'  applied over (`1` indicates rows, `2` indicates columns).
#' @param negate A [`logical`] scalar: should the negation of `f` be used
#'  instead of `f`?
#' @param ... Currently not used.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name count
#' @rdname count
NULL

#' @rdname count
#' @aliases count-method
setGeneric(
  name = "count",
  def = function(x, f, ...) standardGeneric("count")
)

## Detect ----------------------------------------------------------------------
#' Find values according to a given predicate
#'
#' Finds rows/columns in an array-like object according to a given predicate.
#' @inheritParams count
#' @param all A [`logical`] scalar. If `TRUE`, only the rows/columns whose
#'  values all meet the condition defined by `f` are considered. If `FALSE`
#'  (the default), only rows/columns where at least one value validates the
#'  condition defined by `f` are considered.
#' @param ... Currently not used.
#' @return A [`logical`] vector.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name detect
#' @rdname detect
NULL

#' @rdname detect
#' @aliases detect-method
setGeneric(
  name = "detect",
  def = function(x, f, ...) standardGeneric("detect")
)

## Replace ---------------------------------------------------------------------
#' Data Replacement
#'
#' Replaces [`missing`][NA] or [`infinite`][is.finite()] values or zeros.
#' @param x A [`matrix`], a [`data.frame`] or a `*Matrix` object.
#' @param value A possible value to replace missing or infinite values of `x`.
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name replace
#' @rdname replace
NULL

#' @rdname replace
#' @aliases replace_NA-method
setGeneric(
  name = "replace_NA",
  def = function(x, ...) standardGeneric("replace_NA")
)

#' @rdname replace
#' @aliases replace_Inf-method
setGeneric(
  name = "replace_Inf",
  def = function(x, ...) standardGeneric("replace_Inf")
)

#' @rdname replace
#' @aliases replace_zero-method
setGeneric(
  name = "replace_zero",
  def = function(x, ...) standardGeneric("replace_zero")
)

## Remove ----------------------------------------------------------------------
#' Remove values according to a given predicate
#'
#' Removes rows/columns in an array-like object according to a given predicate.
#' @inheritParams detect
#' @param ... Currently not used.
#' @details
#'  * `remove_NA()` remove rows/columns that contain [missing values][NA].
#'  * `remove_Inf()` remove rows/columns that contain [infinite values][is.finite].
#'  * `remove_zero()` remove rows/columns that contain zero.
#'  * `remove_empty()` is a special case that remove empty rows/columns.
#'    A row/column is empty if it contains only `NA`, zeros (if of type
#'    `numeric`) or zero length character strings (if of type `character`).
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name remove
#' @rdname remove
NULL

#' @rdname remove
#' @aliases compact-method
setGeneric(
  name = "compact",
  def = function(x, f, ...) standardGeneric("compact")
)

#' @rdname remove
#' @aliases remove_NA-method
setGeneric(
  name = "remove_NA",
  def = function(x, ...) standardGeneric("remove_NA")
)

#' @rdname remove
#' @aliases remove_Inf-method
setGeneric(
  name = "remove_Inf",
  def = function(x, ...) standardGeneric("remove_Inf")
)

#' @rdname remove
#' @aliases remove_zero-method
setGeneric(
  name = "remove_zero",
  def = function(x, ...) standardGeneric("remove_zero")
)

#' @rdname remove
#' @aliases remove_empty-method
setGeneric(
  name = "remove_empty",
  def = function(x, ...) standardGeneric("remove_empty")
)

# Coerce =======================================================================
#' Coerce
#'
#' Coerces an object to a `*Matrix` object.
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
#'  *Note that `as_count()` rounds numeric values to zero decimal places and
#'  then coerces to integer as by `as.integer()`.*
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
#' @inheritSection AbundanceMatrix-class Abundance Matrix
#' @inheritSection AbundanceMatrix-class Chronology
#' @return A coerced object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family classes
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
#' @param x An object from which to get or set element(s) (typically an
#'  [`AbundanceMatrix-class`] object).
#' @param value A possible value for the element(s) of `x`.
#' @details
#'  \describe{
#'   \item{`get_samples(x)` and `get_samples(x) <- value`}{Get or set
#'   the sample names of `x`.}
#'   \item{`get_groups(x)` and `set_groups(x) <- value`}{Get or set
#'   the groups of `x`.}
#'   \item{`get_dates(x)` and `set_dates(x) <- value`}{Get or set the dates of
#'   `x`.}
#'   \item{`get_terminus(x)` and `set_terminus(x) <- value`}{Get or set
#'   the chronology of `x`. `value` must be a [`list`] with components `tpq`
#'   (TPQ - *terminus post quem*) and `taq` (TAQ - *terminus ante quem*).}
#'   \item{`get_tpq(x)` and `set_tpq(x) <- value`,
#'   `get_taq(x)` and `set_taq(x) <- value`}{Get or set the TPQ/TAQ of `x`.}
#'  }
#' @inheritSection AbundanceMatrix-class Chronology
#' @return
#'  * `set_*()` returns an object of the same sort as `x` with the new values
#'    assigned.
#'  * `get_*()` returns the part of `x`.
#'  * `has_*()` returns a [`logical`] scalar.
#' @example inst/examples/ex-mutators.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

#' @rdname mutators
#' @aliases has_groups-method
setGeneric(
  name = "has_groups",
  def = function(x) standardGeneric("has_groups")
)

#' @rdname mutators
#' @aliases get_groups-method
setGeneric(
  name = "get_groups",
  def = function(x) standardGeneric("get_groups")
)

#' @rdname mutators
#' @aliases set_groups-method
setGeneric(
  name = "set_groups<-",
  def = function(x, value) standardGeneric("set_groups<-")
)

#' @rdname mutators
#' @aliases get_samples-method
setGeneric(
  name = "get_samples",
  def = function(x) standardGeneric("get_samples")
)

#' @rdname mutators
#' @aliases set_samples-method
setGeneric(
  name = "set_samples<-",
  def = function(x, value) standardGeneric("set_samples<-")
)

#' @rdname mutators
#' @aliases has_dates-method
setGeneric(
  name = "has_dates",
  def = function(x) standardGeneric("has_dates")
)

#' @rdname mutators
#' @aliases get_dates-method
setGeneric(
  name = "get_dates",
  def = function(x) standardGeneric("get_dates")
)

#' @rdname mutators
#' @aliases set_dates-method
setGeneric(
  name = "set_dates<-",
  def = function(x, value) standardGeneric("set_dates<-")
)

#' @rdname mutators
#' @aliases has_terminus-method
setGeneric(
  name = "has_terminus",
  def = function(x) standardGeneric("has_terminus")
)

#' @rdname mutators
#' @aliases get_terminus-method
setGeneric(
  name = "get_terminus",
  def = function(x) standardGeneric("get_terminus")
)

#' @rdname mutators
#' @aliases set_terminus-method
setGeneric(
  name = "set_terminus<-",
  def = function(x, value) standardGeneric("set_terminus<-")
)

#' @rdname mutators
#' @aliases get_tpq-method
setGeneric(
  name = "get_tpq",
  def = function(x) standardGeneric("get_tpq")
)

#' @rdname mutators
#' @aliases set_tpq-method
setGeneric(
  name = "set_tpq<-",
  def = function(x, value) standardGeneric("set_tpq<-")
)

#' @rdname mutators
#' @aliases get_taq-method
setGeneric(
  name = "get_taq",
  def = function(x) standardGeneric("get_taq")
)

#' @rdname mutators
#' @aliases set_taq-method
setGeneric(
  name = "set_taq<-",
  def = function(x, value) standardGeneric("set_taq<-")
)

#' @rdname mutators
#' @aliases get_totals-method
setGeneric(
  name = "get_totals",
  def = function(x) standardGeneric("get_totals")
)

#' @rdname mutators
#' @aliases set_totals-method
setGeneric(
  name = "set_totals<-",
  def = function(x, value) standardGeneric("set_totals<-")
)

# Statistics ===================================================================
## Interval ====================================================================
#' Confidence Interval for a Mean
#'
#' Computes a confidence interval for a mean at a desired level of significance.
#' @param object A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param type A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (the default) or
#'  "`normal`". Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @return A length-two [`numeric`] vector giving lower and upper confidence
#'  limits.
#' @example inst/examples/ex-resample.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @rdname confidence
#' @aliases confidence-method
setGeneric(
  name = "confidence",
  def = function(object, ...) standardGeneric("confidence")
)

## Bootstrap -------------------------------------------------------------------


## Jackknife -------------------------------------------------------------------
#' Jackknife Estimation
#'
#' @param object A [`numeric`] vector.
#' @param do A [`function`] that takes `object` as an argument and returns a
#'  single numeric value.
#' @param ... Extra arguments passed to `do`.
#' @return
#'  Returns a named `numeric` vector with the following elements:
#'  \describe{
#'   \item{`mean`}{The jackknife estimate of mean of `do`.}
#'   \item{`bias`}{The jackknife estimate of bias of `do`.}
#'   \item{`error`}{he jackknife estimate of standard error of `do`.}
#'  }
#' @example inst/examples/ex-resample.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @rdname jackknife
#' @aliases jackknife-method
setGeneric(
  name = "jackknife",
  def = function(object, ...) standardGeneric("jackknife")
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
#' @family mutators
#' @name subset
#' @rdname subset
NULL

# Summary ======================================================================
#' Object Summaries
#'
#' Produces result summaries.
#' @param object An [`AbundanceMatrix-class`] object.
#' @param ... Currently not used.
#' @return
#'  An `AbundanceSummary` object.
#' @example inst/examples/ex-mutators.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name summary
#' @rdname summary
NULL
