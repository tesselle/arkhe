# MUTATORS
#' @include AllClasses.R
NULL

#' @export
#' @rdname mutator
#' @aliases length,GenericMatrix-method
setMethod(
  f = "length",
  signature = signature(x = "GenericMatrix"),
  definition = function(x) prod(x@size)
)
#' @export
#' @rdname mutator
#' @aliases dim,GenericMatrix-method
setMethod(
  f = "dim",
  signature = signature(x = "GenericMatrix"),
  definition = function(x) x@size
)
#' @export
#' @rdname mutator
#' @aliases rownames,GenericMatrix-method
setMethod(
  f = "rownames",
  signature = signature(x = "GenericMatrix"),
  definition = function(x) {
    if (length(x@row_names) > 0) x@row_names else NULL
  }
)
#' @export
#' @rdname mutator
#' @aliases rownames<-,GenericMatrix,ANY-method
setMethod(
  f = "rownames<-",
  signature = signature(x = "GenericMatrix", value = "ANY"),
  definition = function(x, value) {
    x@row_names <- as.character(value)
    methods::validObject(x)
    x
  }
)
#' @export
#' @rdname mutator
#' @aliases colnames,GenericMatrix-method
setMethod(
  f = "colnames",
  signature = signature(x = "GenericMatrix"),
  definition = function(x) {
    if (length(x@column_names) > 0) x@column_names else NULL
  }
)
#' @export
#' @rdname mutator
#' @aliases colnames<-,GenericMatrix,ANY-method
setMethod(
  f = "colnames<-",
  signature = signature(x = "GenericMatrix", value = "ANY"),
  definition = function(x, value) {
    x@column_names <- as.character(value)
    methods::validObject(x)
    x
  }
)
#' @export
#' @rdname mutator
#' @aliases dimnames,GenericMatrix-method
setMethod(
  f = "dimnames",
  signature = signature(x = "GenericMatrix"),
  definition = function(x) list(rownames(x), colnames(x))
)
#' @export
#' @rdname mutator
#' @aliases dimnames<-,GenericMatrix,list-method
setMethod(
  f = "dimnames<-",
  signature = signature(x = "GenericMatrix", value = "list"),
  definition = function(x, value) {
    x@row_names <- as.character(value[[1L]])
    x@column_names <- as.character(value[[2L]])
    methods::validObject(x)
    x
  }
)
#' @export
#' @rdname mutator
#' @aliases diag,GenericMatrix-method
setMethod(
  f = "diag",
  signature = signature(x = "GenericMatrix"),
  definition = function(x, names = TRUE) {
    m <- as_matrix(x)
    diag(m, names = names)
  }
)
#' @export
#' @rdname mutator
#' @aliases diag<-,GenericMatrix-method
setMethod(
  f = "diag<-",
  signature = signature(x = "GenericMatrix"),
  definition = function(x, value) {
    m <- as_matrix(x)
    value <- methods::as(value, typeof(x@values))
    diag(m) <- value
    x@values <- m
    methods::validObject(x)
    x
  }
)

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases get_samples,GenericMatrix-method
setMethod("get_samples", "GenericMatrix", function(x) x@sample_names)

#' @export
#' @rdname mutator
#' @aliases get_sites,GenericMatrix-method
setMethod("get_sites", "GenericMatrix", function(x) x@site_names)

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

#' @export
#' @rdname mutator
#' @aliases get_method,SimilarityMatrix-method
setMethod("get_method", "SimilarityMatrix", function(x) x@method)

# Setters ======================================================================
#' @export
#' @rdname mutator
#' @aliases set_samples,GenericMatrix-method
setMethod(
  f = "set_samples<-",
  signature = "GenericMatrix",
  definition = function(x, value) {
    if (!is.factor(value)) value <- factor(value, levels = unique(value))
    x@sample_names <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_sites,GenericMatrix-method
setMethod(
  f = "set_sites<-",
  signature = "GenericMatrix",
  definition = function(x, value) {
    if (!is.factor(value)) value <- factor(value, levels = unique(value))
    x@site_names <- value
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
