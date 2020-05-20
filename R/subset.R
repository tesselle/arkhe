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
    m <- methods::callNextMethod()
    if (!is.matrix(m)) return(x@values[m])
    # Deal with extra slots in child class
    k <- which(rownames(x) %in% rownames(m))
    slots <- methods::slotNames(x)[-1] # Remove .Data
    for (s in slots) {
      old <- methods::slot(x, s)
      n <- length(old)
      if (n == length(x)) value <- old[m]
      else if (n == nrow(x)) value <- old[k]
      else next()
      if (is.factor(old)) value <- droplevels(value)
      if (!is.null(dim(old))) dim(value) <- dim(m)
      methods::slot(x, s, check = TRUE) <- value
    }
    m[] <- seq_along(m)
    methods::initialize(x, m)
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix-method
setMethod(
  f = "[[",
  signature = "DataMatrix",
  definition = function(x, i, j, ..., exact = TRUE) {
    m <- methods::callNextMethod()
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
    m <- if (nargs() > 3) x@.Data[i, j] else x@.Data[i]
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
