# DATA CLEANING: KEEP
#' @include AllGenerics.R
NULL

# keep =========================================================================
#' @export
#' @rdname keep
#' @aliases keep,ANY,function-method
setMethod(
  f = "keep",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE) {
    i <- detect(x, f = f, margin = margin, negate = negate, all = all)
    if (margin == 1) return(x[i, , drop = FALSE])
    if (margin == 2) return(x[, i, drop = FALSE])
    x
  }
)

#' @export
#' @rdname keep
#' @aliases keep_rows,ANY,function-method
setMethod(
  f = "keep_rows",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, negate = FALSE, all = FALSE) {
    keep(x, f, margin = 1, negate = negate, all = all)
  }
)

#' @export
#' @rdname keep
#' @aliases keep_cols,ANY,function-method
setMethod(
  f = "keep_cols",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, negate = FALSE, all = FALSE) {
    keep(x, f, margin = 2, negate = negate, all = all)
  }
)
