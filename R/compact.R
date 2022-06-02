# DATA CLEANING
#' @include AllClasses.R AllGenerics.R
NULL

# Compact ======================================================================
#' @export
#' @rdname remove
#' @aliases compact,ANY,function-method
setMethod(
  f = "compact",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE) {
    i <- !detect(x, f = f, margin = margin, negate = negate, all = all)
    if (margin == 1) return(x[i, , drop = FALSE])
    if (margin == 2) return(x[, i, drop = FALSE])
    x
  }
)

## Missing values --------------------------------------------------------------
#' @export
#' @rdname remove
#' @aliases remove_NA,ANY-method
setMethod(
  f = "remove_NA",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    compact(x, f = is.na, margin = margin, all = all)
  }
)

## Infinite values -------------------------------------------------------------
#' @export
#' @rdname remove
#' @aliases remove_Inf,ANY-method
setMethod(
  f = "remove_Inf",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    compact(x, f = is.infinite, margin = margin, all = all)
  }
)

## Zeros -----------------------------------------------------------------------
#' @export
#' @rdname remove
#' @aliases remove_zero,ANY-method
setMethod(
  f = "remove_zero",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    compact(x, f = is_zero, margin = margin, all = all)
  }
)

## Empty rows/columns ----------------------------------------------------------
#' @export
#' @rdname remove
#' @aliases remove_empty,ANY-method
setMethod(
  f = "remove_empty",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1) {
    vide <- function(x) {
      miss <- is.na(x)
      if (is_numeric(x)) x == 0 | miss
      else if (is_character(x)) x == "" | miss
      else miss
    }
    compact(x, f = vide, margin = margin, all = TRUE)
  }
)
