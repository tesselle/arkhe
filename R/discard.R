# DATA CLEANING: DISCARD
#' @include AllGenerics.R
NULL

# discard ======================================================================
#' @export
#' @rdname discard
#' @aliases discard,ANY,function-method
setMethod(
  f = "discard",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE, ...) {
    i <- !detect(x, f = f, margin = margin, negate = negate, all = all, ...)
    if (margin == 1) return(x[i, , drop = FALSE])
    if (margin == 2) return(x[, i, drop = FALSE])
    x
  }
)

#' @export
#' @rdname discard
#' @aliases discard_rows,ANY,function-method
setMethod(
  f = "discard_rows",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, negate = FALSE, all = FALSE, ...) {
    discard(x, f, margin = 1, negate = negate, all = all, ...)
  }
)

#' @export
#' @rdname discard
#' @aliases discard_cols,ANY,function-method
setMethod(
  f = "discard_cols",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, negate = FALSE, all = FALSE, ...) {
    discard(x, f, margin = 2, negate = negate, all = all, ...)
  }
)
