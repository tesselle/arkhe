# GENERIC METHODS

# Data preparation =============================================================
## Count -----------------------------------------------------------------------
#' Count Values Using a Predicate
#'
#' Counts values by rows/columns using a predicate function.
#' @param x An \R object (should be a [`matrix`] or a [`data.frame`]).
#' @param f A predicate [`function`].
#' @param margin A length-one [`numeric`] vector giving the subscripts which the
#'  function will be applied over (`1` indicates rows, `2` indicates columns).
#' @param negate A [`logical`] scalar: should the negation of `f` be used
#'  instead of `f`?
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param ... Further arguments to be passed to `f`.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-count.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
#' @aliases count-method
setGeneric(
  name = "count",
  def = function(x, ...) standardGeneric("count")
)

## Detect ----------------------------------------------------------------------
#' Find Rows/Columns Using a Predicate
#'
#' Finds rows/columns in an array-like object using a predicate function.
#' @inheritParams count
#' @param all A [`logical`] scalar. If `TRUE`, only the rows/columns whose
#'  values all meet the condition defined by `f` are considered. If `FALSE`
#'  (the default), only rows/columns where at least one value validates the
#'  condition defined by `f` are considered.
#' @param ... Further arguments to be passed to `f`.
#' @return A [`logical`] vector.
#' @example inst/examples/ex-detect.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
#' @aliases detect-method
setGeneric(
  name = "detect",
  def = function(x, ...) standardGeneric("detect")
)

## Keep ------------------------------------------------------------------------
#' Keep Rows/Columns Using a Predicate
#'
#' Keeps rows/columns in an array-like object using a predicate function.
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @example inst/examples/ex-keep.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
#' @aliases keep-method
setGeneric(
  name = "keep",
  def = function(x, ...) standardGeneric("keep")
)

#' @rdname keep
#' @aliases keep_cols-method
setGeneric(
  name = "keep_cols",
  def = function(x, ...) standardGeneric("keep_cols")
)

#' @rdname keep
#' @aliases keep_rows-method
setGeneric(
  name = "keep_rows",
  def = function(x, ...) standardGeneric("keep_rows")
)

## Discard ---------------------------------------------------------------------
#' Remove Rows/Columns Using a Predicate
#'
#' Removes rows/columns in an array-like object using a predicate function.
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @example inst/examples/ex-discard.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
#' @aliases discard-method
setGeneric(
  name = "discard",
  def = function(x, ...) standardGeneric("discard")
)

#' @rdname discard
#' @aliases discard_cols-method
setGeneric(
  name = "discard_cols",
  def = function(x, ...) standardGeneric("discard_cols")
)

#' @rdname discard
#' @aliases discard_rows-method
setGeneric(
  name = "discard_rows",
  def = function(x, ...) standardGeneric("discard_rows")
)

## Compact ---------------------------------------------------------------------
#' Remove Empty Rows/Columns
#'
#' Removes empty rows/columns in an array-like object.
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @details
#'  A row/column is empty if it contains only zeros (if of type `numeric`)
#'  or zero length character strings (if of type `character`).
#' @example inst/examples/ex-compact.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
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

## Seek ------------------------------------------------------------------------
#' Search Rows/Columns by Name
#'
#' Searches rows/columns by name in an array-like object.
#' @param x An \R object (should be a [`matrix`] or a [`data.frame`]).
#' @param select A [`function`] to be applied to the row/colum names
#'  (e.g. [startsWith()]). Must return a single `integer` or `logical` vector.
#' @param ... Further arguments to be passed to `select`.
#' @return An [`integer`] vector or `NULL`.
#' @example inst/examples/ex-seek.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
#' @name seek
#' @rdname seek
NULL

#' @rdname seek
#' @aliases seek_columns-method
setGeneric(
  name = "seek_columns",
  def = function(x, ...) standardGeneric("seek_columns")
)

#' @rdname seek
#' @aliases seek_rows-method
setGeneric(
  name = "seek_rows",
  def = function(x, ...) standardGeneric("seek_rows")
)

## Seek ------------------------------------------------------------------------
#' Get Rows/Columns by Name
#'
#' Returns rows/columns selected by name in an array-like object.
#' @inheritParams seek
#' @return An object of the same sort as `x`.
#' @example inst/examples/ex-seek.R
#' @author N. Frerebeau
#' @docType methods
#' @family data preparation tools
#' @name get
#' @rdname get
NULL

#' @rdname get
#' @aliases get_columns-method
setGeneric(
  name = "get_columns",
  def = function(x, ...) standardGeneric("get_columns")
)

#' @rdname get
#' @aliases get_rows-method
setGeneric(
  name = "get_rows",
  def = function(x, ...) standardGeneric("get_rows")
)

## Assign ----------------------------------------------------------------------
#' Assign a Specific Row/Column to the Column/Row Names
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
#' @family data preparation tools
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

#' Convert Row Names to an Explicit Column
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
#' @family data preparation tools
#' @name append
#' @rdname append
NULL

#' @rdname append
#' @aliases append_rownames-method
setGeneric(
  name = "append_rownames",
  def = function(x, ...) standardGeneric("append_rownames")
)

# Data cleaning ================================================================
## NA --------------------------------------------------------------------------
#' Remove Rows/Columns with Missing Values
#'
#' Removes rows/columns that contain [missing values][NA].
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @example inst/examples/ex-missing.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases remove_NA-method
#' @aliases missing
setGeneric(
  name = "remove_NA",
  def = function(x, ...) standardGeneric("remove_NA")
)

#' Replace Missing Values
#'
#' Replaces [missing values][NA] values.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-missing.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases replace_NA-method
setGeneric(
  name = "replace_NA",
  def = function(x, ...) standardGeneric("replace_NA")
)

## Inf -------------------------------------------------------------------------
#' Remove Rows/Columns with Infinite Values
#'
#' Removes rows/columns that contain [infinite values][is.finite].
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @example inst/examples/ex-infinite.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases remove_Inf-method
setGeneric(
  name = "remove_Inf",
  def = function(x, ...) standardGeneric("remove_Inf")
)

#' Replace Infinite Values
#'
#' Replaces [infinite values][is.finite] values.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-infinite.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases replace_Inf-method
setGeneric(
  name = "replace_Inf",
  def = function(x, ...) standardGeneric("replace_Inf")
)

## Zeros -----------------------------------------------------------------------
#' Remove Rows/Columns with Zeros
#'
#' Removes rows/columns that contain zeros.
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @example inst/examples/ex-zero.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases remove_zero-method
#' @aliases zero
setGeneric(
  name = "remove_zero",
  def = function(x, ...) standardGeneric("remove_zero")
)

#' Replace Zeros
#'
#' Replaces zeros.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-zero.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases replace_zero-method
setGeneric(
  name = "replace_zero",
  def = function(x, ...) standardGeneric("replace_zero")
)

## Empty string ----------------------------------------------------------------
#' Remove Rows/Columns with Empty String
#'
#' Removes rows/columns that contain empty strings.
#' @inheritParams detect
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @example inst/examples/ex-empty.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases remove_empty-method
setGeneric(
  name = "remove_empty",
  def = function(x, ...) standardGeneric("remove_empty")
)

#' Replace Empty String
#'
#' Replaces empty strings.
#' @inheritParams detect
#' @param value A possible replacement value.
#' @param ... Currently not used.
#' @example inst/examples/ex-empty.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases replace_empty-method
setGeneric(
  name = "replace_empty",
  def = function(x, ...) standardGeneric("replace_empty")
)

## Constant --------------------------------------------------------------------
#' Remove Constant Columns
#'
#' @param x An \R object (should be a [`matrix`] or a [`data.frame`]).
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @example inst/examples/ex-constant.R
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases remove_constant-method
setGeneric(
  name = "remove_constant",
  def = function(x, ...) standardGeneric("remove_constant")
)

## Whitespace ------------------------------------------------------------------
#' Remove Leading/Trailing Whitespace
#'
#' @param x An \R object (should be a [`matrix`] or a [`data.frame`]).
#' @param which A [`character`] string specifying whether to remove `both`
#'  leading and trailing whitespace (default), or only leading ("`left`") or
#'  trailing ("`right`").
#' @param squish A [`logical`] scalar: should all internal whitespace be
#'  replaced with a single space?
#' @param ... Currently not used.
#' @example inst/examples/ex-whitespace.R
#' @seealso [trimws()]
#' @author N. Frerebeau
#' @docType methods
#' @family data cleaning tools
#' @aliases clean_whitespace-method
setGeneric(
  name = "clean_whitespace",
  def = function(x, ...) standardGeneric("clean_whitespace")
)

# Data summary =================================================================
#' Data Description
#'
#' Describes an object.
#' @param x An \R object (should be a [`matrix`] or a [`data.frame`]).
#' @param ... Currently not used.
#' @return
#'  `describe()` is called for its side-effects. Invisibly returns `x`.
#' @example inst/examples/ex-describe.R
#' @author N. Frerebeau
#' @docType methods
#' @family data summaries
#' @aliases describe-method
setGeneric(
  name = "describe",
  def = function(x, ...) standardGeneric("describe")
)

#' Sparsity
#'
#' Computes data sparsity (proportion of zeros).
#' @param x An \R object (should be a [`matrix`] or a [`data.frame`]).
#' @param count A [`logical`] scalar: should a count be returned instead of a
#'  proportion?
#' @param ... Currently not used.
#' @details
#'  If `x` is a `data.frame`, sparsity is computed on `numeric` variables only.
#' @return
#'  A length-one [`numeric`] vector.
#' @example inst/examples/ex-describe.R
#' @author N. Frerebeau
#' @docType methods
#' @family data summaries
#' @aliases sparsity-method
setGeneric(
  name = "sparsity",
  def = function(x, ...) standardGeneric("sparsity")
)

# Data transformation ==========================================================

# Mathematics ==================================================================
#' Least Common Multiple
#'
#' Computes the lowest common multiple of the denominators of a set of
#' fractions.
#' @param x,y A [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family mathematic functions
#' @aliases math_lcm-method
setGeneric(
  name = "math_lcm",
  def = function(x, y) standardGeneric("math_lcm")
)

#' Greatest Common Divisor
#'
#' Computes the greatest common divisor (GCD) of two integer using the Euclidean
#' algorithm.
#' @param x,y A [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family mathematic functions
#' @aliases math_gcd-method
setGeneric(
  name = "math_gcd",
  def = function(x, y) standardGeneric("math_gcd")
)

# Statistics ===================================================================
## Interval --------------------------------------------------------------------
#' Highest Density Regions
#'
#' @param x A [`numeric`] vector giving the coordinates of the points where
#'  the density is estimated.
#' @param y A [`numeric`] vector giving the estimated density values.
#'  If `y` is missing and `x` is a `numeric` vector, density estimates will be
#'  computed from `x`.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Further arguments to be passed to [stats::density()].
#' @return
#'  A three-columns `numeric` [`matrix`] giving the lower and upper boundaries
#'  of the HPD interval and associated probabilities.
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
#' @example inst/examples/ex-interval.R
#' @author N. Frerebeau
#' @family summary statistics
#' @docType methods
#' @aliases interval_hdr-method
setGeneric(
  name = "interval_hdr",
  def = function(x, y, ...) standardGeneric("interval_hdr")
)

#' Bayesian Credible Interval
#'
#' Computes the shortest credible interval within which an unobserved parameter
#' value falls with a particular probability.
#' @param x A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @return
#'  A three-columns `numeric` [`matrix`] giving the lower and upper boundaries
#'  of the credible interval and associated probability.
#' @example inst/examples/ex-interval.R
#' @author N. Frerebeau
#' @family summary statistics
#' @docType methods
#' @aliases interval_credible-method
setGeneric(
  name = "interval_credible",
  def = function(x, ...) standardGeneric("interval_credible")
)

## Confidence ------------------------------------------------------------------
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

# Resampling ===================================================================
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
#'  If `f` is `NULL` (the default), `bootstrap()` returns a named `numeric`
#'  vector with the following elements:
#'  \describe{
#'   \item{`original`}{The observed value of `do` applied to `object`.}
#'   \item{`mean`}{The bootstrap estimate of mean of `do`.}
#'   \item{`bias`}{The bootstrap estimate of bias of `do`.}
#'   \item{`error`}{he bootstrap estimate of standard error of `do`.}
#'  }
#'
#'  If `f` is a `function`, `bootstrap()` returns the result of `f` applied to
#'  the `n` values of `do`.
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
#' @param f A [`function`] that takes a single numeric vector (the leave-one-out
#'  values of `do`) as argument.
#' @return
#'  If `f` is `NULL` (the default), `jackknife()` returns a named `numeric`
#'  vector with the following elements:
#'  \describe{
#'   \item{`original`}{The observed value of `do` applied to `object`.}
#'   \item{`mean`}{The jackknife estimate of mean of `do`.}
#'   \item{`bias`}{The jackknife estimate of bias of `do`.}
#'   \item{`error`}{he jackknife estimate of standard error of `do`.}
#'  }
#'
#'  If `f` is a `function`, `jackknife()` returns the result of `f` applied to
#'  the leave-one-out values of `do`.
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
