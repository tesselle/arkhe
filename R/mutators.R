# MUTATORS
#' @include AllClasses.R
NULL

# ====================================================================== Getters
#' @export
#' @rdname mutator
#' @aliases get_id,GenericMatrix-method
setMethod("get_id", "GenericMatrix", function(x) x@id)

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

#' @export
#' @rdname mutator
#' @aliases get_method,SimilarityMatrix-method
setMethod("get_method", "SimilarityMatrix", function(x) x@method)

# ====================================================================== Setters
#' @export
#' @rdname mutator
#' @aliases set_id,GenericMatrix-method
setMethod(
  f = "set_id<-",
  signature = "GenericMatrix",
  definition = function(x, value) {
    x@id <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_totals,AbundanceMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "AbundanceMatrix",
  definition = function(x, value) {
    x@totals <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_method,SimilarityMatrix-method
setMethod(
  f = "set_method<-",
  signature = "SimilarityMatrix",
  definition = function(x, value) {
    x@method <- value
    methods::validObject(x)
    x
  }
)
