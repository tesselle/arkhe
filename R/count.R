# DATA CLEANING: COUNT
#' @include AllGenerics.R
NULL

# Count ========================================================================
#' @export
#' @rdname count
#' @aliases count,data.frame,function-method
setMethod(
  f = "count",
  signature = signature(x = "data.frame", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, ...) {
    if (negate) f <- Negate(f)
    x[] <- vapply(X = x, FUN = f, FUN.VALUE = logical(nrow(x)), ...)
    if (margin == 1) return(rowSums(x))
    if (margin == 2) return(colSums(x))
    x
  }
)

#' @export
#' @rdname count
#' @aliases count,matrix,function-method
setMethod(
  f = "count",
  signature = signature(x = "matrix", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, ...) {
    x <- as.data.frame(x)
    methods::callGeneric(x, f = f, margin = margin, negate = negate, ...)
  }
)
