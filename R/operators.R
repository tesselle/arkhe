# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname operator
#' @aliases ==,CountMatrix,CountMatrix-method
setMethod(
  f = "==",
  signature(e1 = "CountMatrix", e2 = "CountMatrix"),
  definition = function (e1, e2) {
    methods::as(e1, "matrix") == methods::as(e2, "matrix")
  }
)

#' @export
#' @rdname operator
#' @aliases -,CountMatrix,CountMatrix-method
setMethod(
  f = "-",
  signature(e1 = "CountMatrix", e2 = "CountMatrix"),
  definition = function (e1, e2) {
    methods::as(e1, "matrix") - methods::as(e2, "matrix")
  }
)

#' @export
#' @rdname operator
#' @aliases -,CountMatrix,numeric-method
setMethod(
  f = "-",
  signature(e1 = "CountMatrix", e2 = "numeric"),
  definition = function (e1, e2) {
    methods::as(e1, "matrix") - e2
  }
)

#' @export
#' @rdname operator
#' @aliases +,CountMatrix,CountMatrix-method
setMethod(
  f = "+",
  signature(e1 = "CountMatrix", e2 = "CountMatrix"),
  definition = function (e1, e2) {
    methods::as(e1, "matrix") + methods::as(e2, "matrix")
  }
)

#' @export
#' @rdname operator
#' @aliases +,CountMatrix,numeric-method
setMethod(
  f = "+",
  signature(e1 = "CountMatrix", e2 = "numeric"),
  definition = function (e1, e2) {
    methods::as(e1, "matrix") + e2
  }
)

#' @export
#' @rdname operator
#' @aliases rowSums,DataMatrix-method
setMethod(
  f = "rowSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    rowSums(methods::as(x, "matrix"), na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases colSums,DataMatrix-method
setMethod(
  f = "colSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    colSums(methods::as(x, "matrix"), na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases rowMeans,DataMatrix-method
setMethod(
  f = "rowMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    rowMeans(methods::as(x, "matrix"), na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases colMeans,DataMatrix-method
setMethod(
  f = "colMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    colMeans(methods::as(x, "matrix"), na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases rowRanges,DataMatrix-method
setMethod(
  f = "rowRanges",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    index <- row(x, as.factor = TRUE)
    data <- split(x = x@data, f = index)
    vapply(X = data, FUN = range, FUN.VALUE = numeric(2), na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases colRanges,DataMatrix-method
setMethod(
  f = "colRanges",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    index <- col(x, as.factor = TRUE)
    data <- split(x = x@data, f = index)
    vapply(X = data, FUN = range, FUN.VALUE = numeric(2), na.rm = na.rm)
  }
)
