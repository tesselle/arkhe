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
#' @aliases has_dates,AbundanceMatrix-method
setMethod("has_dates", "AbundanceMatrix", function(x) length(x@dates_from) > 0)

#' @export
#' @rdname mutator
#' @aliases get_dates,AbundanceMatrix-method
setMethod("get_dates", "AbundanceMatrix", function(x) {
  y <- data.frame(
    from = x@dates_from,
    to = x@dates_to
  )
  if (!is_empty(y)) rownames(y) <- rownames(x)
  y
})

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

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
#' @aliases set_dates,AbundanceMatrix,NULL-method
setMethod(
  f = "set_dates<-",
  signature = c(x = "AbundanceMatrix", value = "NULL"),
  definition = function(x, value) {
    x@dates_from <- integer(0)
    x@dates_to <- integer(0)

    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_dates,AbundanceMatrix,numeric-method
setMethod(
  f = "set_dates<-",
  signature = c(x = "AbundanceMatrix", value = "numeric"),
  definition = function(x, value) {
    ## Coerce to integer
    x@dates_from <- as.integer(value)
    x@dates_to <- as.integer(value)

    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_dates,AbundanceMatrix,list-method
setMethod(
  f = "set_dates<-",
  signature = c(x = "AbundanceMatrix", value = "list"),
  definition = function(x, value) {
    ## Validation
    k <- match(c("from", "to"), names(value))
    if (anyNA(k)) {
      msg <- sprintf("%s is a list, but does not have components %s and %s.",
                     sQuote("x"), sQuote("from"), sQuote("to"))
      stop(msg, call. = FALSE)
    }

    ## Coerce to integer
    x@dates_from <- as.integer(value$from)
    x@dates_to <- as.integer(value$to)

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
