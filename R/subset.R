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
  definition = function(x, i, j, ..., drop = TRUE) {
    m <- callNextMethod()
    if (!is.matrix(m)) return(x@values[m])
    methods::initialize(x, m, values = x@values[m])
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix-method
setMethod(
  f = "[[",
  signature = "DataMatrix",
  definition = function(x, i, j, ..., exact = TRUE) {
    m <- callNextMethod()
    x@values[[m]]
  }
)

# ====================================================================== Replace
#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix-method
setMethod(
  f = "[<-",
  signature = "DataMatrix",
  definition = function(x, i, j, ..., value) {
    if (nargs() == 4) m <- x@.Data[i, j]
    if (nargs() == 3) m <- x@.Data[i]
    value <- methods::as(value, typeof(x@values))
    x@values[m] <- value
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
    m <- if (missing(j)) x@.Data[[i]] else x@.Data[[i, j]]
    value <- methods::as(value, typeof(x@values))
    x@values[[m]] <- value
    methods::validObject(x)
    x
  }
)
