# GENERIC METHODS
#' @importFrom methods setGeneric setMethod .valueClassTest
NULL

# Data cleaning ================================================================
## Count -----------------------------------------------------------------------
#' Count values using a predicate
#'
#' Counts values by rows/columns using a predicate function.
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
#' @aliases count-method
setGeneric(
  name = "count",
  def = function(x, f, ...) standardGeneric("count")
)

## Detect ----------------------------------------------------------------------
#' Find rows/columns using a predicate
#'
#' Finds rows/columns in an array-like object using a predicate function.
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
#' @aliases detect-method
setGeneric(
  name = "detect",
  def = function(x, f, ...) standardGeneric("detect")
)

## Keep ------------------------------------------------------------------------
#' Keep rows/columns using a predicate
#'
#' Keeps rows/columns in an array-like object using a predicate function.
#' @inheritParams detect
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases keep-method
setGeneric(
  name = "keep",
  def = function(x, f, ...) standardGeneric("keep")
)

#' @rdname keep
#' @aliases keep_cols-method
setGeneric(
  name = "keep_cols",
  def = function(x, f, ...) standardGeneric("keep_cols")
)

#' @rdname keep
#' @aliases keep_rows-method
setGeneric(
  name = "keep_rows",
  def = function(x, f, ...) standardGeneric("keep_rows")
)

## Discard ---------------------------------------------------------------------
#' Remove rows/columns using a predicate
#'
#' Removes rows/columns in an array-like object using a predicate function.
#' @inheritParams detect
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases discard-method
setGeneric(
  name = "discard",
  def = function(x, f, ...) standardGeneric("discard")
)

#' @rdname discard
#' @aliases discard_cols-method
setGeneric(
  name = "discard_cols",
  def = function(x, f, ...) standardGeneric("discard_cols")
)

#' @rdname discard
#' @aliases discard_rows-method
setGeneric(
  name = "discard_rows",
  def = function(x, f, ...) standardGeneric("discard_rows")
)

## Compact ---------------------------------------------------------------------
#' Remove empty rows/columns
#'
#' Removes empty rows/columns in an array-like object using a predicate
#' function.
#' @inheritParams detect
#' @param ... Currently not used.
#' @details
#'  A row/column is empty if it contains only `NA`, zeros (if of type `numeric`)
#'  or zero length character strings (if of type `character`).
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases compact-method
setGeneric(
  name = "compact",
  def = function(x, ...) standardGeneric("compact")
)

#' @rdname compact
#' @aliases compact_cols-method
setGeneric(
  name = "compact_cols",
  def = function(x, ...) standardGeneric("compact_cols")
)

#' @rdname compact
#' @aliases compact_rows-method
setGeneric(
  name = "compact_rows",
  def = function(x, ...) standardGeneric("compact_rows")
)

## Remove ----------------------------------------------------------------------
### NA --------------------------------------------------------------------------
#' Tools for working with missing values
#'
#' @description
#'  * `remove_NA()` remove rows/columns that contain [missing values][NA].
#'  * `replace_NA` replaces [missing values][NA] values.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name missing
#' @rdname missing
NULL

#' @rdname missing
#' @aliases remove_NA-method
setGeneric(
  name = "remove_NA",
  def = function(x, ...) standardGeneric("remove_NA")
)

#' @rdname missing
#' @aliases replace_NA-method
setGeneric(
  name = "replace_NA",
  def = function(x, ...) standardGeneric("replace_NA")
)

### Inf -------------------------------------------------------------------------
#' Tools for working with infinite values
#'
#' @description
#'  * `remove_Inf()` remove rows/columns that contain [infinite values][is.finite].
#'  * `replace_Inf` replaces [infinite values][is.finite] values.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name infinite
#' @rdname infinite
NULL

#' @rdname infinite
#' @aliases remove_Inf-method
setGeneric(
  name = "remove_Inf",
  def = function(x, ...) standardGeneric("remove_Inf")
)

#' @rdname infinite
#' @aliases replace_Inf-method
setGeneric(
  name = "replace_Inf",
  def = function(x, ...) standardGeneric("replace_Inf")
)

### Zeros -----------------------------------------------------------------------
#' Tools for working with zeros
#'
#' @description
#'  * `remove_zero()` remove rows/columns that contain zeros.
#'  * `replace_zero` replaces zeros.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-clean.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @name zero
#' @rdname zero
NULL

#' @rdname zero
#' @aliases remove_zero-method
setGeneric(
  name = "remove_zero",
  def = function(x, ...) standardGeneric("remove_zero")
)

#' @rdname zero
#' @aliases replace_zero-method
setGeneric(
  name = "replace_zero",
  def = function(x, ...) standardGeneric("replace_zero")
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
#' @example inst/examples/ex-assign.R
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

#' Convert row names to an explicit column
#'
#' @param x A [`data.frame`].
#' @param after A length-one [`numeric`] vector specifying a subscript,
#'  after which the row names are to be appended.
#' @param var A [`character`] string giving the name of column to use for row
#'  names.
#' @param remove A [`logical`] scalar: should the row names be removed?
#' @param ... Currently not used.
#' @example inst/examples/ex-assign.R
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @docType methods
#' @family transformation tools
#' @name append
#' @rdname append
NULL

#' @rdname append
#' @aliases append_rownames-method
setGeneric(
  name = "append_rownames",
  def = function(x, ...) standardGeneric("append_rownames")
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
#' @example inst/examples/ex-reshape.R
#' @author N. Frerebeau
#' @docType methods
#' @family transformation tools
#' @name reshape
#' @rdname reshape
NULL

#' @rdname reshape
#' @aliases wide_to_long-method
setGeneric(
  name = "wide_to_long",
  def = function(from, ...) standardGeneric("wide_to_long"),
  valueClass = "data.frame"
)

#' @rdname reshape
#' @aliases to_long-method
setGeneric(
  name = "to_long",
  def = function(from, ...) standardGeneric("to_long"),
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
#' @return A length-two [`numeric`] vector giving the lower and upper confidence
#'  limits.
#' @example inst/examples/ex-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary statistics
#' @aliases confidence_mean-method
setGeneric(
  name = "confidence_mean",
  def = function(object, ...) standardGeneric("confidence_mean")
)

#' Confidence Interval for Binomial Proportions
#'
#' Computes a Wald interval for a proportion at a desired level of significance.
#' @param object A [`numeric`] vector giving the number of success.
#' @param n A length-one [`numeric`] vector giving the number of trials.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param method A [`character`] string specifying the method to be used.
#'  Any unambiguous substring can be used.
#' @param corrected A [`logical`] scalar: should continuity correction be used?
#'  Only used if `method` is "`wald`".
#' @param ... Currently not used.
#' @return A length-two [`numeric`] vector giving the lower and upper confidence
#'  limits.
#' @example inst/examples/ex-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary statistics
#' @aliases confidence_binomial-method
setGeneric(
  name = "confidence_binomial",
  def = function(object, ...) standardGeneric("confidence_binomial")
)

#' Confidence Interval for Multinomial Proportions
#'
#' Computes a Wald interval for a proportion at a desired level of significance.
#' @param object A [`numeric`] vector of positive integers giving the number of
#'  occurrences of each class.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param method A [`character`] string specifying the method to be used.
#'  Any unambiguous substring can be used.
#' @param corrected A [`logical`] scalar: should continuity correction be used?
#'  Only used if `method` is "`wald`".
#' @param ... Currently not used.
#' @return A two column [`numeric`] `matrix` giving the lower and upper
#'  confidence limits.
#' @example inst/examples/ex-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary statistics
#' @aliases confidence_multinomial-method
setGeneric(
  name = "confidence_multinomial",
  def = function(object, ...) standardGeneric("confidence_multinomial")
)

## Bootstrap -------------------------------------------------------------------
#' Bootstrap Estimation
#'
#' Samples randomly from the elements of `object` with replacement.
#' @param object A [`numeric`] vector.
#' @param do A [`function`] that takes `object` as an argument and returns a
#'  single numeric value.
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  `do`) as argument.
#' @param ... Extra arguments to be passed to `do`.
#' @return
#'  If `f` is not `NULL`, `bootstrap()` returns the result of `f` applied to
#'  the `n` values of `do`.
#'
#'  If `f` is `NULL`, `bootstrap()` returns a named `numeric` `vector` with the
#'  following elements:
#'  \describe{
#'   \item{`original`}{The observed value of `do` applied to `object`.}
#'   \item{`mean`}{The bootstrap estimate of mean of `do`.}
#'   \item{`bias`}{The bootstrap estimate of bias of `do`.}
#'   \item{`error`}{he bootstrap estimate of standard error of `do`.}
#'  }
#' @example inst/examples/ex-resample.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @rdname bootstrap
#' @aliases bootstrap-method
setGeneric(
  name = "bootstrap",
  def = function(object, ...) standardGeneric("bootstrap")
)

## Jackknife -------------------------------------------------------------------
#' Jackknife Estimation
#'
#' @param object A [`numeric`] vector.
#' @param do A [`function`] that takes `object` as an argument and returns a
#'  single numeric value.
#' @param ... Extra arguments to be passed to `do`.
#' @return
#'  Returns a named `numeric` `vector` with the following elements:
#'  \describe{
#'   \item{`original`}{The observed value of `do` applied to `object`.}
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

# Deprecated ===================================================================
#' Deprecated Methods
#'
#' @param object A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param type A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (the default) or
#'  "`normal`". Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @author N. Frerebeau
#' @docType methods
#' @name deprecate
#' @rdname deprecate
#' @keywords internal
NULL
