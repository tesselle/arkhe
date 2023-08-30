# DATA CLEANING: DETECT
#' @include AllGenerics.R
NULL

# Detect =======================================================================
#' @export
#' @rdname detect
#' @aliases detect,ANY-method
setMethod(
  f = "detect",
  signature = c(x = "ANY"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE, ...) {
    total <- count(x, f, margin = margin, negate = negate, ...)
    k <- 1
    if (all) {
      if (any(margin == 1)) k <- ncol(x)
      if (any(margin == 2)) k <- nrow(x)
    }
    total >= k
  }
)
