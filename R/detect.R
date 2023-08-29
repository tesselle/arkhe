# DATA CLEANING: DETECT
#' @include AllGenerics.R
NULL

# Detect =======================================================================
#' @export
#' @rdname detect
#' @aliases detect,ANY,function-method
setMethod(
  f = "detect",
  signature = signature(x = "ANY", f = "function"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE, ...) {
    total <- count(x, f, margin = margin, negate = negate, ...)
    k <- 1
    if (all) {
      if (margin == 1) k <- ncol(x)
      if (margin == 2) k <- nrow(x)
    }
    total >= k
  }
)
