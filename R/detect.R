# DATA CLEANING: DETECT
#' @include AllClasses.R AllGenerics.R
NULL

# Count ========================================================================
#' @export
#' @rdname count
#' @aliases count,matrix,function-method
setMethod(
  f = "count",
  signature = signature(x = "matrix", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE) {
    if (negate) f <- Negate(f)
    x <- f(x)
    if (margin == 1) return(rowSums(x))
    if (margin == 2) return(colSums(x))
    x
  }
)

#' @export
#' @rdname count
#' @aliases count,data.frame,function-method
setMethod(
  f = "count",
  signature = signature(x = "data.frame", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE) {
    if (negate) f <- Negate(f)
    x[] <- lapply(X = x, FUN = f)
    if (margin == 1) return(rowSums(x))
    if (margin == 2) return(colSums(x))
    x
  }
)

# Detect =======================================================================
#' @export
#' @rdname detect
#' @aliases detect,ANY,function-method
setMethod(
  f = "detect",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE) {
    total <- count(x, f, margin = margin, negate = negate)
    k <- 1
    if (all) {
      if (margin == 1) k <- ncol(x)
      if (margin == 2) k <- nrow(x)
    }
    total >= k
  }
)
