# MUTATORS
#' @include AllClasses.R
NULL

#' @export
#' @rdname mutator
#' @aliases length,DataMatrix-method
setMethod(
  f = "length",
  signature = signature(x = "DataMatrix"),
  definition = function(x) prod(x@size)
)
#' @export
#' @rdname mutator
#' @aliases dim,DataMatrix-method
setMethod(
  f = "dim",
  signature = signature(x = "DataMatrix"),
  definition = function(x) x@size
)
#' @export
#' @rdname mutator
#' @aliases dim,CA-method
setMethod(
  f = "dim",
  signature = signature(x = "CA"),
  definition = function(x) x@dimension
)
#' @export
#' @rdname mutator
#' @aliases rownames,DataMatrix-method
setMethod(
  f = "rownames",
  signature = signature(x = "DataMatrix"),
  definition = function(x) {
    if (length(x@row_names) > 0) x@row_names else NULL
  }
)
#' @export
#' @rdname mutator
#' @aliases rownames<-,DataMatrix,ANY-method
setMethod(
  f = "rownames<-",
  signature = signature(x = "DataMatrix", value = "ANY"),
  definition = function(x, value) {
    x@row_names <- make.unique(as.character(value), sep = "_")
    methods::validObject(x)
    x
  }
)
#' @export
#' @rdname mutator
#' @aliases colnames,DataMatrix-method
setMethod(
  f = "colnames",
  signature = signature(x = "DataMatrix"),
  definition = function(x) {
    if (length(x@column_names) > 0) x@column_names else NULL
  }
)
#' @export
#' @rdname mutator
#' @aliases colnames<-,DataMatrix,ANY-method
setMethod(
  f = "colnames<-",
  signature = signature(x = "DataMatrix", value = "ANY"),
  definition = function(x, value) {
    x@column_names <- make.unique(as.character(value), sep = "_")
    methods::validObject(x)
    x
  }
)
#' @export
#' @rdname mutator
#' @aliases dimnames,DataMatrix-method
setMethod(
  f = "dimnames",
  signature = signature(x = "DataMatrix"),
  definition = function(x) list(rownames(x), colnames(x))
)
#' @export
#' @rdname mutator
#' @aliases dimnames<-,DataMatrix,list-method
setMethod(
  f = "dimnames<-",
  signature = signature(x = "DataMatrix", value = "list"),
  definition = function(x, value) {
    x@row_names <- as.character(value[[1L]])
    x@column_names <- as.character(value[[2L]])
    methods::validObject(x)
    x
  }
)
#' @export
#' @rdname mutator
#' @aliases diag,DataMatrix-method
setMethod(
  f = "diag",
  signature = signature(x = "DataMatrix"),
  definition = function(x) {
    m <- as.matrix(x)
    diag(m)
  }
)
#' @export
#' @rdname mutator
#' @aliases diag<-,DataMatrix-method
setMethod(
  f = "diag<-",
  signature = signature(x = "DataMatrix"),
  definition = function(x, value) {
    m <- as.matrix(x)
    diag(m) <- value
    m <- methods::as(m, typeof(x@values))
    x@values <- m
    methods::validObject(x)
    x
  }
)

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases has_groups,DataMatrix-method
setMethod("has_groups", "DataMatrix", function(x) length(x@group_names) > 0)

#' @export
#' @rdname mutator
#' @aliases get_groups,DataMatrix-method
setMethod("get_groups", "DataMatrix", function(x) x@group_names)

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

#' @export
#' @rdname mutator
#' @aliases get_totals,OccurrenceMatrix-method
setMethod("get_totals", "OccurrenceMatrix", function(x) x@n)

#' @export
#' @rdname mutator
#' @aliases get_method,SimilarityMatrix-method
setMethod("get_method", "SimilarityMatrix", function(x) x@method)

#' @export
#' @rdname mutator
#' @aliases get_n,OccurrenceMatrix-method
setMethod("get_n", "OccurrenceMatrix", function(x) x@n)

# Setters ======================================================================
#' @export
#' @rdname mutator
#' @aliases set_groups,DataMatrix-method
setMethod(
  f = "set_groups<-",
  signature = "DataMatrix",
  definition = function(x, value) {
    x@group_names <- if (is.null(value)) character(0) else as.character(value)
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
