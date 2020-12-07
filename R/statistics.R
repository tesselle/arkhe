# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname statistics
#' @aliases rowMeans,DataMatrix-method
setMethod(
  f = "rowMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    methods::callGeneric(x = as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colMeans,DataMatrix-method
setMethod(
  f = "colMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    methods::callGeneric(x = as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases rowSums,DataMatrix-method
setMethod(
  f = "rowSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    methods::callGeneric(x = as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colSums,DataMatrix-method
setMethod(
  f = "colSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    methods::callGeneric(x = as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname correlation
#' @aliases var,DataMatrix-method
setMethod(
  f = "var",
  signature = signature(x = "DataMatrix", y = "missing"),
  definition = function(x, na.rm = FALSE) {
    methods::callGeneric(x = as.matrix(x), na.rm = na.rm)
  }
)

#' @export
#' @rdname correlation
#' @aliases cov,DataMatrix-method
setMethod(
  f = "cov",
  signature = signature(x = "DataMatrix", y = "missing"),
  definition = function(x, use = "everything",
                        method = c("pearson", "kendall", "spearman")) {
    methods::callGeneric(x = as.matrix(x), use = use, method = method)
  }
)

#' @export
#' @rdname correlation
#' @aliases cor,DataMatrix-method
setMethod(
  f = "cor",
  signature = signature(x = "DataMatrix", y = "missing"),
  definition = function(x, use = "everything",
                        method = c("pearson", "kendall", "spearman")) {
    methods::callGeneric(x = as.matrix(x), use = use, method = method)
  }
)
