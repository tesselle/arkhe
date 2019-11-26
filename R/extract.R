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
  definition = function(x, i, j, value) {
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
#' @aliases get_totals,FrequencyMatrix-method
setMethod("get_totals", "FrequencyMatrix", function(object) object@totals)

# ====================================================================== Setters
#' @export
#' @rdname mutator
#' @aliases set_totals,FrequencyMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "FrequencyMatrix",
  definition = function(object, value) {
    object@totals <- value
    methods::validObject(object)
    object
  }
)
