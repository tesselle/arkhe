# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,missing,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "missing", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,numeric,missing,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "numeric", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    m <- seq_along(x@values)
    dim(m) <- x@size

    k <- if (nargs() >= 3) m[i, , drop = drop] else m[i]
    if (is.null(dim(k))) return(x@values[k])
    methods::initialize(
      .Object = x,
      size = dim(k),
      row_names = x@row_names[i],
      values = x@values[k]
    )
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,missing,numeric,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "missing", j = "numeric", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    m <- seq_along(x@values)
    dim(m) <- x@size

    k <- m[, j, drop = drop]
    if (is.null(dim(k))) return(x@values[k])
    methods::initialize(
      .Object = x,
      size = dim(k),
      column_names = x@column_names[j],
      sample_names = x@sample_names[j],
      date_values = x@date_values[j],
      date_errors = x@date_errors[j],
      values = x@values[k]
    )
  }
)

#' @export
#' @rdname subset
#' @aliases [,DataMatrix,numeric,numeric,ANY-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix", i = "numeric", j = "numeric", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    m <- seq_along(x@values)
    dim(m) <- x@size

    k <- m[i, j, drop = drop]
    if (is.null(dim(k))) return(x@values[k])
    methods::initialize(
      .Object = x,
      size = dim(k),
      row_names = x@row_names[i],
      column_names = x@column_names[j],
      sample_names = x@sample_names[j],
      date_values = x@date_values[j],
      date_errors = x@date_errors[j],
      values = x@values[k]
    )
  }
)

## [[ --------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[,DataMatrix,numeric,missing-method
setMethod(
  f = "[[",
  signature = c(x = "DataMatrix", i = "numeric", j = "missing"),
  function(x, i, j, ...) {
    m <- seq_along(x@values)
    dim(m) <- x@size

    k <-if (nargs() > 2) m[i, ] else m[i]
    x@values[[k]]
  }
)

#' @export
#' @rdname subset
#' @aliases [[,DataMatrix,numeric,numeric-method
setMethod(
  f = "[[",
  signature = c(x = "DataMatrix", i = "numeric", j = "numeric"),
  function(x, i, j, ...) {
    m <- seq_along(x@values)
    dim(m) <- x@size

    k <- m[[i, j]]
    x@values[[k]]
  }
)

# Replace ======================================================================
## [<- -------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix,missing,missing,ANY-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "missing", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {
    value <- methods::as(value, typeof(x@values))
    x@values  <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix,numeric,missing,ANY-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "numeric", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {
    value <- methods::as(value, typeof(x@values))
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- if (nargs() > 3) m[i, ] else m[i]

    x@values[k]  <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix,missing,numeric,ANY-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "missing", j = "numeric", value = "ANY"),
  function(x, i, j, ..., value) {
    value <- methods::as(value, typeof(x@values))
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- m[, j]

    x@values[k]  <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix,numeric,numeric,ANY-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix", i = "numeric", j = "numeric", value = "ANY"),
  function(x, i, j, ..., value) {
    value <- methods::as(value, typeof(x@values))
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- m[i, j]

    x@values[k]  <- value
    methods::validObject(x)
    x
  }
)

## [[<- ------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[<-,DataMatrix,numeric,missing,ANY-method
setMethod(
  f = "[[<-",
  signature = c(x = "DataMatrix", i = "numeric", j = "missing", value = "ANY"),
  function(x, i, j, ..., value) {
    value <- methods::as(value, typeof(x@values))
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- if (nargs() > 3) m[[i, ]] else m[[i]]

    x@values[[k]]  <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [[<-,DataMatrix,numeric,numeric,ANY-method
setMethod(
  f = "[[<-",
  signature = c(x = "DataMatrix", i = "numeric", j = "numeric", value = "ANY"),
  function(x, i, j, ..., value) {
    value <- methods::as(value, typeof(x@values))
    m <- seq_along(x@values)
    dim(m) <- x@size
    k <- m[[i, j]]

    x@values[[k]]  <- value
    methods::validObject(x)
    x
  }
)
