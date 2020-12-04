# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

#' Geometric Mean
#'
#' @param x A \code{\link{numeric}} vector.
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
gmean <- function(x, na.rm = FALSE) {
  index <- if (na.rm) is.finite(x) & x > 0 else x > 0
  exp(mean(log(unclass(x)[index])))
}

#' @export
#' @rdname statistics
#' @aliases mean,AbundanceMatrix-method
setMethod(
  f = "mean",
  signature = signature(x = "AbundanceMatrix"),
  definition = function(x, na.rm = FALSE) {
    x <- apply(X = x, MARGIN = 2, FUN = gmean, na.rm = na.rm)
    x / sum(x)
  }
)

#' @export
#' @rdname statistics
#' @aliases rowMeans,DataMatrix-method
setMethod(
  f = "rowMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    rowMeans(as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colMeans,DataMatrix-method
setMethod(
  f = "colMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    colMeans(as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases rowSums,DataMatrix-method
setMethod(
  f = "rowSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    rowSums(as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colSums,DataMatrix-method
setMethod(
  f = "colSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    colSums(as.matrix(x), na.rm = na.rm)
  }
)

