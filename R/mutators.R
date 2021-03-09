# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases has_groups,AbundanceMatrix-method
setMethod("has_groups", "AbundanceMatrix", function(x) length(x@groups) > 0)

#' @export
#' @rdname mutator
#' @aliases get_groups,AbundanceMatrix-method
setMethod("get_groups", "AbundanceMatrix", function(x) x@groups)

#' @export
#' @rdname mutator
#' @aliases get_samples,AbundanceMatrix-method
setMethod("get_samples", "AbundanceMatrix", function(x) x@samples)

#' @export
#' @rdname mutator
#' @aliases get_totals,CompositionMatrix-method
setMethod("get_totals", "CompositionMatrix", function(x) x@total)

#' @export
#' @rdname mutator
#' @aliases get_totals,OccurrenceMatrix-method
setMethod("get_totals", "OccurrenceMatrix", function(x) x@total)

# Setters ======================================================================
#' @export
#' @rdname mutator
#' @aliases set_groups,AbundanceMatrix-method
setMethod(
  f = "set_groups<-",
  signature = "AbundanceMatrix",
  definition = function(x, value) {
    x@groups <- if (is.null(value)) character(0) else as.character(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_samples,AbundanceMatrix-method
setMethod(
  f = "set_samples<-",
  signature = "AbundanceMatrix",
  definition = function(x, value) {
    x@samples <- if (is.null(value)) rownames(x) else as.character(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_totals,CompositionMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    x@total <- value
    methods::validObject(x)
    x
  }
)
