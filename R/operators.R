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
