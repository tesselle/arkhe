# DATA CLEANING: REMOVE
#' @include AllGenerics.R
NULL

## Missing values ==============================================================
#' @export
#' @rdname missing
#' @aliases remove_NA,ANY-method
setMethod(
  f = "remove_NA",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    discard(x, f = is.na, margin = margin, all = all)
  }
)

## Infinite values =============================================================
#' @export
#' @rdname infinite
#' @aliases remove_Inf,ANY-method
setMethod(
  f = "remove_Inf",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    discard(x, f = is.infinite, margin = margin, all = all)
  }
)

## Zeros =======================================================================
#' @export
#' @rdname zero
#' @aliases remove_zero,ANY-method
setMethod(
  f = "remove_zero",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE, ...) {
    discard(x, f = is_zero, margin = margin, all = all, ...)
  }
)
