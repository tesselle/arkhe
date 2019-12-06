# ACCESSORS
#' @include AllClasses.R
NULL

# ====================================================================== Replace
#' @export
#' @rdname subset
#' @aliases [,Matrix-method
setMethod(
  f = "[<-",
  signature = "Matrix",
  definition = function(x, i, value) {
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

#' @export
#' @rdname subset
#' @aliases [,Matrix-method
setMethod(
  f = "[<-",
  signature = "Matrix",
  definition = function(x, i, j, ..., value) {
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

#' @export
#' @rdname subset
#' @aliases [[,Matrix-method
setMethod(
  f = "[[<-",
  signature = "Matrix",
  definition = function(x, i, value) {
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

# ====================================================================== Getters
#' @export
#' @rdname mutator
#' @aliases get_id,ANY-method
setMethod("get_id", "ANY", function(object) object@id)

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(object) object@totals)

#' @export
#' @rdname mutator
#' @aliases get_method,SimilarityMatrix-method
setMethod("get_method", "SimilarityMatrix", function(object) object@method)

# ====================================================================== Setters
#' @export
#' @rdname mutator
#' @aliases set_totals,AbundanceMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    object@totals <- value
    methods::validObject(object)
    object
  }
)
