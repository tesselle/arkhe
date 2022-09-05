# GENERIC METHODS
NULL

# Data cleaning ================================================================
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

# Data transformation ==========================================================
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
#' @family transformation tools
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

## Reshape ---------------------------------------------------------------------
#' Reshape
#'
#' Transforms a `matrix` to a long `data.frame`.
#' @param from An object to be coerced.
#' @param factor A [`logical`] scalar: should character string be
#'  coerced to [`factor`]? Default to `FALSE`, if `TRUE` the original ordering is
#'  preserved.
#' @param reverse A [`logical`] scalar: should the order of factor levels be
#'  reversed? Only used if `factor` is `TRUE`. Useful for plotting.
#' @param ... Currently not used.
#' @return A coerced object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family transformation tools
#' @name reshape
#' @rdname reshape
NULL

#' @rdname reshape
#' @aliases as_long-method
setGeneric(
  name = "as_long",
  def = function(from, ...) standardGeneric("as_long"),
  valueClass = "data.frame"
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
