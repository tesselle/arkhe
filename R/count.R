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
  definition = function(x, f, margin = 1, negate = FALSE, na.rm = FALSE, ...) {
    if (negate) f <- Negate(f)
    x <- lapply(X = x, FUN = f, ...)
    x <- do.call(cbind.data.frame, x)
    if (any(margin == 1)) return(rowSums(x, na.rm = na.rm))
    if (any(margin == 2)) return(colSums(x, na.rm = na.rm))
    x
  }
)

#' @export
#' @rdname count
#' @aliases count,matrix,function-method
setMethod(
  f = "count",
  signature = signature(x = "matrix", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, na.rm = FALSE, ...) {
    x <- as.data.frame(x)
    methods::callGeneric(x, f = f, margin = margin, negate = negate,
                         na.rm = na.rm, ...)
  }
)
