# ACCESSORS
#' @include AllClasses.R
NULL

# ====================================================================== Extract
#' @export
#' @rdname subset
#' @aliases [,DataMatrix-method
setMethod(
  f = "[",
  signature = "DataMatrix",
  definition = function(x, i, j, drop = TRUE) {
    mtx <- methods::as(x, "matrix")
    mtx[i, j, drop = drop]
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix-method
setMethod(
  f = "[[",
  signature = "DataMatrix",
  definition = function(x, i, j) {
    mtx <- methods::as(x, "matrix")
    mtx[[i, j]]
  }
)

# ====================================================================== Replace
#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix-method
setMethod(
  f = "[<-",
  signature = "DataMatrix",
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
#' @aliases [[<-,DataMatrix-method
setMethod(
  f = "[[<-",
  signature = "DataMatrix",
  definition = function(x, i, j, value) {
    mtx <- methods::as(x, "matrix")
    mtx[[i, j]] <- value
    mtx <- methods::as(mtx, typeof(x@data))
    x@data <- mtx
    methods::validObject(x)
    x
  }
)
