# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases has_groups,AbundanceMatrix-method
setMethod("has_groups", "AbundanceMatrix", function(x) !is_empty(x@groups))

#' @export
#' @rdname mutators
#' @aliases get_groups,AbundanceMatrix-method
setMethod("get_groups", "AbundanceMatrix", function(x) x@groups)

#' @export
#' @rdname mutators
#' @aliases get_samples,AbundanceMatrix-method
setMethod("get_samples", "AbundanceMatrix", function(x) x@samples)

#' @export
#' @rdname mutators
#' @aliases has_dates,AbundanceMatrix-method
setMethod("has_dates", "AbundanceMatrix", function(x) !is_empty(x@dates))

#' @export
#' @rdname mutators
#' @aliases get_dates,AbundanceMatrix-method
setMethod("get_dates", "AbundanceMatrix", function(x) x@dates)

#' @export
#' @rdname mutators
#' @aliases has_terminus,AbundanceMatrix-method
setMethod("has_terminus", "AbundanceMatrix", function(x) {
  !is_empty(x@tpq) && !is_empty(x@taq)
})

#' @export
#' @rdname mutators
#' @aliases get_terminus,AbundanceMatrix-method
setMethod("get_terminus", "AbundanceMatrix", function(x) {
  y <- data.frame(
    tpq = x@tpq,
    taq = x@taq
  )
  if (!is_empty(y)) rownames(y) <- rownames(x)
  y
})

#' @export
#' @rdname mutators
#' @aliases get_tpq,AbundanceMatrix-method
setMethod("get_tpq", "AbundanceMatrix", function(x) x@tpq)

#' @export
#' @rdname mutators
#' @aliases get_taq,AbundanceMatrix-method
setMethod("get_taq", "AbundanceMatrix", function(x) x@taq)

#' @export
#' @rdname mutators
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

#' @export
#' @rdname mutators
#' @aliases get_totals,OccurrenceMatrix-method
setMethod("get_totals", "OccurrenceMatrix", function(x) x@total)

# Setters ======================================================================
#' @export
#' @rdname mutators
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
#' @rdname mutators
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
#' @rdname mutators
#' @aliases set_dates,AbundanceMatrix,NULL-method
setMethod(
  f = "set_dates<-",
  signature = c(x = "AbundanceMatrix", value = "NULL"),
  definition = function(x, value) {
    x@dates <- integer(0)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_dates,AbundanceMatrix,numeric-method
setMethod(
  f = "set_dates<-",
  signature = c(x = "AbundanceMatrix", value = "numeric"),
  definition = function(x, value) {
    x@dates <- as.integer(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_terminus,AbundanceMatrix,NULL-method
setMethod(
  f = "set_terminus<-",
  signature = c(x = "AbundanceMatrix", value = "NULL"),
  definition = function(x, value) {
    x@tpq <- integer(0)
    x@taq <- integer(0)

    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_terminus,AbundanceMatrix,list-method
setMethod(
  f = "set_terminus<-",
  signature = c(x = "AbundanceMatrix", value = "list"),
  definition = function(x, value) {
    ## Validation
    names(value) <- tolower(names(value))
    k <- match(c("tpq", "taq"), names(value))
    if (anyNA(k)) {
      msg <- sprintf("%s is a list, but does not have components %s and %s.",
                     sQuote("x"), sQuote("tpq"), sQuote("taq"))
      stop(msg, call. = FALSE)
    }

    ## Coerce to integer
    x@tpq <- as.integer(value$tpq)
    x@taq <- as.integer(value$taq)

    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_tpq,AbundanceMatrix,NULL-method
setMethod(
  f = "set_tpq<-",
  signature = c(x = "AbundanceMatrix", value = "NULL"),
  definition = function(x, value) {
    x@tpq <- integer(0)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_tpq,AbundanceMatrix,numeric-method
setMethod(
  f = "set_tpq<-",
  signature = c(x = "AbundanceMatrix", value = "numeric"),
  definition = function(x, value) {
    x@tpq <- as.integer(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_taq,AbundanceMatrix,NULL-method
setMethod(
  f = "set_taq<-",
  signature = c(x = "AbundanceMatrix", value = "NULL"),
  definition = function(x, value) {
    x@taq <- integer(0)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_taq,AbundanceMatrix,numeric-method
setMethod(
  f = "set_taq<-",
  signature = c(x = "AbundanceMatrix", value = "numeric"),
  definition = function(x, value) {
    x@taq <- as.integer(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
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
