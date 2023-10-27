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
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE,
                        na.rm = FALSE, ...) {
    total <- count(x, f, margin = margin, negate = negate, na.rm = na.rm, ...)

    miss <- rep(0, length(total))
    if (na.rm) miss <- count(x, f = is.na, margin = margin)

    k <- 1
    if (all) {
      if (any(margin == 1)) k <- ncol(x) - miss
      if (any(margin == 2)) k <- nrow(x) - miss
    }
    total >= k
  }
)
