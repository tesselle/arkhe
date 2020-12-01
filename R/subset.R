# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
#' @export
#' @rdname subset
#' @aliases [,DataMatrix,numeric,numeric,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "numeric", j = "numeric", drop = "ANY"),
  definition = function(x, i, j, ..., drop = TRUE) {
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- m[i, j, drop = drop]

    if (is.null(dim(k))) return(x@values[k])
    methods::initialize(
      .Object = x,
      size = dim(k),
      row_names = x@row_names[i],
      column_names = x@column_names[j],
      values = x@values[k]
    )
  }
)
#' @export
#' @rdname subset
#' @aliases [,DataMatrix,numeric,missing,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "numeric", j = "missing", drop = "ANY"),
  definition = function(x, i, ..., drop = TRUE) {
    if (nargs() == 2) {
      x@values[i]
    } else {
      m <- seq_along(x@values)
      dim(m) <- x@size
      k <- m[i, , drop = drop]

      if (is.null(dim(k))) return(x@values[k])
      methods::initialize(
        .Object = x,
        size = dim(k),
        row_names = x@row_names[i],
        values = x@values[k]
      )
    }
  }
)
#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,numeric,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "missing", j = "numeric", drop = "ANY"),
  definition = function(x, j, ..., drop = TRUE) {
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- m[, j, drop = drop]

    if (is.null(dim(k))) return(x@values[k])
    methods::initialize(
      .Object = x,
      size = dim(k),
      column_names = x@column_names[j],
      values = x@values[k]
    )
  }
)
#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,missing,missing-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "missing", j = "missing", drop = "missing"),
  definition = function(x, ...) {
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix,numeric,numeric-method
setMethod(
  f = "[[",
  signature = c(x = "DataMatrix", i = "numeric", j = "numeric"),
  definition = function(x, i, j, ..., exact = TRUE) {
    n <- x@size[[1L]]
    x@values[[(j - 1) * n + i]]
  }
)
#' @export
#' @rdname subset
#' @aliases [[,DataMatrix,numeric,missing-method
setMethod(
  f = "[[",
  signature = c(x = "DataMatrix", i = "numeric", j = "missing"),
  definition = function(x, i, exact = TRUE) {
    x@values[[i]]
  }
)

# Replace ======================================================================
#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix-method
setMethod(
  f = "[<-",
  signature = "DataMatrix",
  definition = function(x, i, j, ..., value) {
    m <- seq_along(x@values)
    dim(m) <- x@size

    if (missing(i) && !missing(j)) k <- m[, j]
    if (missing(j) && !missing(i)) k <- if (nargs() > 3) m[i, ] else m[i]
    if (!missing(i) && !missing(j)) k <- m[i, j]

    value <- methods::as(value, typeof(x@values))
    x@values[k] <- value
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
    value <- methods::as(value, typeof(x@values))
    n <- x@size[[1L]]

    if (missing(j) && !missing(i)) x@values[[i]] <- value
    if (!missing(i) && !missing(j)) x@values[[(j - 1) * n + i]] <- value

    methods::validObject(x)
    x
  }
)
