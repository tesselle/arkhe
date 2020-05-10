# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

# ======================================================================== Arith
#' @export
#' @rdname operator
#' @aliases Arith,DataMatrix,DataMatrix-method
setMethod(
  f = "Arith",
  signature(e1 = "DataMatrix", e2 = "DataMatrix"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@data, e2 = e2@data)
    dim(value) <- dim(e1)
    return(value)
  }
)

#' @export
#' @rdname operator
#' @aliases Arith,DataMatrix,numeric-method
setMethod(
  f = "Arith",
  signature(e1 = "DataMatrix", e2 = "numeric"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@data, e2 = e2)
    dim(value) <- dim(e1)
    return(value)
  }
)

# ====================================================================== Compare
#' @export
#' @rdname operator
#' @aliases Compare,DataMatrix,DataMatrix-method
setMethod(
  f = "Compare",
  signature(e1 = "DataMatrix", e2 = "DataMatrix"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@data, e2 = e2@data)
    dim(value) <- dim(e1)
    return(value)
  }
)

#' @export
#' @rdname operator
#' @aliases Compare,DataMatrix,numeric-method
setMethod(
  f = "Compare",
  signature(e1 = "DataMatrix", e2 = "numeric"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@data, e2 = e2)
    dim(value) <- dim(e1)
    return(value)
  }
)

# ======================================================================== Logic
#' @export
#' @rdname operator
#' @aliases Logic,DataMatrix,DataMatrix-method
setMethod(
  f = "Logic",
  signature(e1 = "DataMatrix", e2 = "DataMatrix"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@data, e2 = e2@data)
    dim(value) <- dim(e1)
    return(value)
  }
)

#' @export
#' @rdname operator
#' @aliases Logic,DataMatrix,numeric-method
setMethod(
  f = "Logic",
  signature(e1 = "DataMatrix", e2 = "numeric"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@data, e2 = e2)
    dim(value) <- dim(e1)
    return(value)
  }
)

# ====================================================================== Summary
#' @export
#' @rdname operator
#' @aliases Summary,DataMatrix-method
setMethod(
  f = "Summary",
  signature(x = "DataMatrix"),
  definition = function(x, na.rm = FALSE) {
    methods::callGeneric(x = x@data, na.rm = na.rm)
  }
)

# ------------------------------------------------------------------------------
#' @export
#' @rdname operator
#' @aliases rowAll,DataMatrix-method
setMethod(
  f = "rowAll",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    fun <- function(x, ..., na.rm) all(f(x, ...), na.rm = na.rm)
    apply(X = x, MARGIN = 1, FUN = fun, ..., na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases colAll,DataMatrix-method
setMethod(
  f = "colAll",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    fun <- function(x, ..., na.rm) all(f(x, ...), na.rm = na.rm)
    apply(X = x, MARGIN = 2, FUN = fun, ..., na.rm = na.rm)
  }
)

# ------------------------------------------------------------------------------
#' @export
#' @rdname operator
#' @aliases rowAny,DataMatrix-method
setMethod(
  f = "rowAny",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    fun <- function(x, ..., na.rm) any(f(x, ...), na.rm = na.rm)
    apply(X = x, MARGIN = 1, FUN = fun, ..., na.rm = na.rm)
  }
)

#' @export
#' @rdname operator
#' @aliases colAny,DataMatrix-method
setMethod(
  f = "colAny",
  signature = signature(x = "DataMatrix"),
  definition = function(x, f, ..., na.rm = FALSE) {
    fun <- function(x, ..., na.rm) any(f(x, ...), na.rm = na.rm)
    apply(X = x, MARGIN = 2, FUN = fun, ..., na.rm = na.rm)
  }
)
