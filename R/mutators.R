# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases has_groups,DataMatrix-method
setMethod("has_groups", "DataMatrix", function(x) length(x@groups) > 0)

#' @export
#' @rdname mutator
#' @aliases get_groups,DataMatrix-method
setMethod("get_groups", "DataMatrix", function(x) x@groups)

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

#' @export
#' @rdname mutator
#' @aliases get_totals,OccurrenceMatrix-method
setMethod("get_totals", "OccurrenceMatrix", function(x) x@total)

#' @export
#' @rdname mutator
#' @aliases get_method,SimilarityMatrix-method
setMethod("get_method", "SimilarityMatrix", function(x) x@method)

# Setters ======================================================================
#' @export
#' @rdname mutator
#' @aliases set_groups,DataMatrix-method
setMethod(
  f = "set_groups<-",
  signature = "DataMatrix",
  definition = function(x, value) {
    x@groups <- if (is.null(value)) character(0) else as.character(value)
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
