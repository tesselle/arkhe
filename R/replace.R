# DATA CLEANING: REPLACE
#' @include AllGenerics.R
NULL

# Replace ======================================================================
## Missing values --------------------------------------------------------------
#' @export
#' @rdname missing
#' @aliases replace_NA,matrix-method
setMethod(
  f = "replace_NA",
  signature = signature(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.na(x)] <- value
    x
  }
)

## Infinite values -------------------------------------------------------------
#' @export
#' @rdname infinite
#' @aliases replace_Inf,matrix-method
setMethod(
  f = "replace_Inf",
  signature = signature(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.infinite(x)] <- value
    x
  }
)

## Zeros -----------------------------------------------------------------------
#' @export
#' @rdname zero
#' @aliases replace_zero,matrix-method
setMethod(
  f = "replace_zero",
  signature = signature(x = "matrix"),
  definition = function(x, value) {
    x[is_zero(x)] <- value
    x
  }
)
