# ACCESSORS
#' @include AllClasses.R
NULL

# ====================================================================== Extract
#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,missing-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "missing", j = "missing"),
  definition = function(x, i, j, ..., drop = TRUE) {
    methods::as(x, "matrix")
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,ANY,missing-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "ANY", j = "missing"),
  definition = function(x, i, j, ..., drop = TRUE) {
    n <- nargs() + missing(drop)
    mtx <- methods::as(x, "matrix")
    if (n == 4) {
      mtx[i, , drop = drop]
    } else {
      mtx[i]
    }
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "missing", j = "ANY"),
  definition = function(x, i, j, ..., drop = TRUE) {
    mtx <- methods::as(x, "matrix")
    mtx[, j, drop = drop]
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,ANY,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "ANY", j = "ANY"),
  definition = function(x, i, j, ..., drop = TRUE) {
    mtx <- methods::as(x, "matrix")
    mtx[i, j, drop = drop]
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "DataMatrix", i = "ANY", j = "missing"),
  definition = function(x, i, exact = TRUE) {
    mtx <- methods::as(x, "matrix")
    mtx[[i, exact = exact]]
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix,ANY,ANY-method
setMethod(
  f = "[[",
  signature = c(x = "DataMatrix", i = "ANY", j = "ANY"),
  definition = function(x, i, j, ..., exact = TRUE) {
    mtx <- methods::as(x, "matrix")
    mtx[[i, j, exact = exact]]
  }
)

# ====================================================================== Replace
#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix,ANY,missing-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "ANY", j = "missing"),
  definition = function(x, i, j, ..., value) {
    n <- nargs()
    mtx <- methods::as(x, "matrix")
    if (n == 4) {
      mtx[i, j] <- value
    } else {
      mtx[i] <- value
    }
    mtx <- methods::as(mtx, typeof(x@data))
    x@data <- mtx
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,ANY-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "missing", j = "ANY"),
  definition = function(x, i, j, value) {
    mtx <- methods::as(x, "matrix")
    mtx[, j] <- value
    mtx <- methods::as(mtx, typeof(x@data))
    x@data <- mtx
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix,ANY,ANY-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "ANY", j = "ANY"),
  definition = function(x, i, j, value) {
    mtx <- methods::as(x, "matrix")
    mtx[i, j] <- value
    mtx <- methods::as(mtx, typeof(x@data))
    x@data <- mtx
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [[<-,DataMatrix,ANY,missing-method
setMethod(
  f = "[[<-",
  signature = c(x = "DataMatrix", i = "ANY", j = "missing"),
  definition = function(x, i, value) {
    mtx <- methods::as(x, "matrix")
    mtx[[i]] <- value
    mtx <- methods::as(mtx, typeof(x@data))
    x@data <- mtx
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [[<-,DataMatrix,ANY,ANY-method
setMethod(
  f = "[[<-",
  signature = c(x = "DataMatrix", i = "ANY", j = "ANY"),
  definition = function(x, i, j, value) {
    mtx <- methods::as(x, "matrix")
    mtx[[i, j]] <- value
    mtx <- methods::as(mtx, typeof(x@data))
    x@data <- mtx
    methods::validObject(x)
    x
  }
)
