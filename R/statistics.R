# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

# ------------------------------------------------------------------------------
#' @export
#' @rdname statistics
#' @aliases rowAll,DataMatrix-method
setMethod(
  f = "rowAll",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    data <- as_list(x, by_row = TRUE)
    fun <- function(x, ..., na.rm) all(f(x, ...), na.rm = na.rm)
    vapply(X = data, FUN = fun, FUN.VALUE = logical(1), ..., na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colAll,DataMatrix-method
setMethod(
  f = "colAll",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    data <- as_list(x, by_row = FALSE)
    fun <- function(x, ..., na.rm) all(f(x, ...), na.rm = na.rm)
    vapply(X = data, FUN = fun, FUN.VALUE = logical(1), ..., na.rm = na.rm)
  }
)

# ------------------------------------------------------------------------------
#' @export
#' @rdname statistics
#' @aliases rowAny,DataMatrix-method
setMethod(
  f = "rowAny",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    data <- as_list(x, by_row = TRUE)
    fun <- function(x, ..., na.rm) any(f(x, ...), na.rm = na.rm)
    vapply(X = data, FUN = fun, FUN.VALUE = logical(1), ..., na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colAny,DataMatrix-method
setMethod(
  f = "colAny",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    data <- as_list(x, by_row = FALSE)
    fun <- function(x, ..., na.rm) any(f(x, ...), na.rm = na.rm)
    vapply(X = data, FUN = fun, FUN.VALUE = logical(1), ..., na.rm = na.rm)
  }
)

# ------------------------------------------------------------------------------
#' @export
#' @rdname statistics
#' @aliases rowQuantiles,DataMatrix-method
setMethod(
  f = "rowQuantiles",
  signature = signature(x = "DataMatrix"),
  definition = function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
                        names = TRUE, type = 7, ...) {
    n <- length(probs)
    data <- as_list(x, by_row = TRUE)
    vapply(X = data, FUN = stats::quantile, FUN.VALUE = numeric(n),
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
    n <- length(probs)
    data <- as_list(x, by_row = FALSE)
    vapply(X = data, FUN = stats::quantile, FUN.VALUE = numeric(n),
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
    rowMeans(methods::as(x, "matrix"), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colMeans,DataMatrix-method
setMethod(
  f = "colMeans",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    colMeans(methods::as(x, "matrix"), na.rm = na.rm)
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
    data <- as_list(x, by_row = TRUE)
    vapply(X = data, FUN = range, FUN.VALUE = numeric(2), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colRanges,DataMatrix-method
setMethod(
  f = "colRanges",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    data <- as_list(x, by_row = FALSE)
    vapply(X = data, FUN = range, FUN.VALUE = numeric(2), na.rm = na.rm)
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
    rowSums(methods::as(x, "matrix"), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colSums,DataMatrix-method
setMethod(
  f = "colSums",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    colSums(methods::as(x, "matrix"), na.rm = na.rm)
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
    data <- as_list(x, by_row = TRUE)
    vapply(X = data, FUN = stats::var, FUN.VALUE = numeric(1), na.rm = na.rm)
  }
)

#' @export
#' @rdname statistics
#' @aliases colVars,DataMatrix-method
setMethod(
  f = "colVars",
  signature = signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    data <- as_list(x, by_row = FALSE)
    vapply(X = data, FUN = stats::var, FUN.VALUE = numeric(1), na.rm = na.rm)
  }
)
