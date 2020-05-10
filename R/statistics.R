# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

# ------------------------------------------------------------------------------
#' @export
#' @rdname statistics
#' @aliases rowQuantiles,DataMatrix-method
setMethod(
  f = "rowQuantiles",
  signature = signature(x = "DataMatrix"),
  definition = function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                        names = TRUE, type = 7, ...) {
    apply(X = x, MARGIN = 1, FUN = stats::quantile,
          probs = probs, na.rm = na.rm, names = names, type = type, ...)
  }
)

#' @export
#' @rdname statistics
#' @aliases colQuantiles,DataMatrix-method
setMethod(
  f = "colQuantiles",
  signature = signature(x = "DataMatrix"),
  definition = function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                        names = TRUE, type = 7, ...) {
    apply(X = x, MARGIN = 2, FUN = stats::quantile,
          probs = probs, na.rm = na.rm, names = names, type = type, ...)
  }
)

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
#' @export
#' @rdname statistics
#' @aliases rowRanges,DataMatrix-method
setMethod(
  f = "rowRanges",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    apply(X = x, MARGIN = 1, FUN = range, na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colRanges,DataMatrix-method
setMethod(
  f = "colRanges",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    apply(X = x, MARGIN = 2, FUN = range, na.rm = na.rm)
  }
)

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
#' @export
#' @rdname statistics
#' @aliases rowVars,DataMatrix-method
setMethod(
  f = "rowVars",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    apply(X = x, MARGIN = 1, FUN = stats::var, na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colVars,DataMatrix-method
setMethod(
  f = "colVars",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    apply(X = x, MARGIN = 2, FUN = stats::var, na.rm = na.rm)
  }
)
