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
  definition = function(x, i, j, value) {
    x@.Data[i, j] <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname subset
#' @aliases [[,Matrix-method
setMethod(
  f = "[[<-",
  signature = "Matrix",
  definition = function(x, i, j, value) {
    if (missing(j)) {
      x@.Data[[i]] <- value
    } else {
      x@.Data[[i, j]] <- value
    }
    methods::validObject(x)
    x
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

#' @export
#' @rdname mutator
#' @aliases get_units,StratigraphicMatrix-method
setMethod("get_units", "StratigraphicMatrix", function(object) object@units)

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
