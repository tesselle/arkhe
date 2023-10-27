# DATA CLEANING: COUNT
#' @include AllGenerics.R
NULL

# Count ========================================================================
#' @export
#' @rdname count
#' @aliases count,data.frame-method
setMethod(
  f = "count",
  signature = c(x = "data.frame"),
  definition = function(x, f, margin = 1, negate = FALSE, na.rm = FALSE, ...) {
    assert_function(f)
    if (negate) f <- Negate(f)
    x[] <- lapply(X = x, FUN = f, ...)
    if (any(margin == 1)) return(rowSums(x, na.rm = na.rm))
    if (any(margin == 2)) return(colSums(x, na.rm = na.rm))
    x
  }
)

#' @export
#' @rdname count
#' @aliases count,matrix-method
setMethod(
  f = "count",
  signature = c(x = "matrix"),
  definition = function(x, f, margin = 1, negate = FALSE, na.rm = FALSE, ...) {
    assert_function(f)
    if (negate) f <- Negate(f)
    x <- apply(X = x, MARGIN = margin, FUN = f, ...)
    ## apply() returns an array of dimension c(n, dim(X)[MARGIN])
    colSums(x, na.rm = na.rm)
  }
)
