# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

# Arith ========================================================================
#' @export
#' @rdname operator
#' @aliases Arith,DataMatrix,DataMatrix-method
setMethod(
  f = "Arith",
  signature(e1 = "DataMatrix", e2 = "DataMatrix"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = as.matrix(e2))
  }
)

#' @export
#' @rdname operator
#' @aliases Arith,DataMatrix,matrix-method
setMethod(
  f = "Arith",
  signature(e1 = "DataMatrix", e2 = "matrix"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

#' @export
#' @rdname operator
#' @aliases Arith,DataMatrix,numeric-method
setMethod(
  f = "Arith",
  signature(e1 = "DataMatrix", e2 = "numeric"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

#' @export
#' @rdname operator
#' @aliases Arith,DataMatrix,logical-method
setMethod(
  f = "Arith",
  signature(e1 = "DataMatrix", e2 = "logical"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

# Compare ======================================================================
#' @export
#' @rdname operator
#' @aliases Compare,DataMatrix,DataMatrix-method
setMethod(
  f = "Compare",
  signature(e1 = "DataMatrix", e2 = "DataMatrix"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = as.matrix(e2))
  }
)

#' @export
#' @rdname operator
#' @aliases Compare,DataMatrix,matrix-method
setMethod(
  f = "Compare",
  signature(e1 = "DataMatrix", e2 = "matrix"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

#' @export
#' @rdname operator
#' @aliases Compare,DataMatrix,numeric-method
setMethod(
  f = "Compare",
  signature(e1 = "DataMatrix", e2 = "numeric"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

# Logic ========================================================================
#' @export
#' @rdname operator
#' @aliases Logic,DataMatrix,DataMatrix-method
setMethod(
  f = "Logic",
  signature(e1 = "DataMatrix", e2 = "DataMatrix"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = as.matrix(e2))
  }
)

#' @export
#' @rdname operator
#' @aliases Logic,DataMatrix,matrix-method
setMethod(
  f = "Logic",
  signature(e1 = "DataMatrix", e2 = "matrix"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

#' @export
#' @rdname operator
#' @aliases Logic,DataMatrix,numeric-method
setMethod(
  f = "Logic",
  signature(e1 = "DataMatrix", e2 = "numeric"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

#' @export
#' @rdname operator
#' @aliases Logic,DataMatrix,logical-method
setMethod(
  f = "Logic",
  signature(e1 = "DataMatrix", e2 = "logical"),
  definition = function(e1, e2) {
    methods::callGeneric(e1 = as.matrix(e1), e2 = e2)
  }
)

# Math =========================================================================
#' @export
#' @rdname operator
#' @aliases Math,DataMatrix-method
setMethod(
  f = "Math",
  signature(x = "DataMatrix"),
  definition = function(x) {
    methods::callGeneric(x = as.matrix(x))
  }
)

# Math2 ========================================================================
#' @export
#' @rdname operator
#' @aliases Math2,DataMatrix-method
setMethod(
  f = "Math2",
  signature(x = "DataMatrix"),
  definition = function(x, digits) {
    methods::callGeneric(x = as.matrix(x), digits = digits)
  }
)

# Summary ======================================================================
#' @export
#' @rdname operator
#' @aliases Summary,DataMatrix-method
setMethod(
  f = "Summary",
  signature(x = "DataMatrix"),
  definition = function(x, ..., na.rm = FALSE) {
    methods::callGeneric(x = as.matrix(x), na.rm = na.rm)
  }
)
