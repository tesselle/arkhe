# MUTATORS
#' @include AllClasses.R
NULL

#' @export
#' @describeIn mutator Retrieve the length of an object.
#' @aliases length,DataMatrix-method
setMethod(
  f = "length",
  signature = c("DataMatrix"),
  definition = function(x) {
    prod(x@size)
  }
)

#' @export
#' @describeIn mutator Extract the diagonal of a matrix.
#' @aliases diag,DataMatrix-method
setMethod(
  f = "diag",
  signature = c("DataMatrix"),
  definition = function(x) {
    diag(methods::as(x, "matrix"), names = TRUE)
  }
)

#' @export
#' @describeIn mutator Retrieve or set the dimension of an object.
#' @aliases dim,DataMatrix-method
setMethod(
  f = "dim",
  signature = c("DataMatrix"),
  definition = function(x) {
    x@size
  }
)

#' @export
#' @describeIn mutator Returns a matrix of integers indicating their row number
#' in a matrix-like object, or a factor indicating the row labels.
#' @aliases row,DataMatrix-method
setMethod(
  f = "row",
  signature = c("DataMatrix"),
  definition = function(x, as.factor = FALSE) {
    size <- x@size
    idx <- index_by_row(size)
    if (as.factor) idx <- factor(idx, labels = x@row_names)
    matrix(data = idx, nrow = size[[1L]], ncol = size[[2L]])
  }
)

#' @export
#' @describeIn mutator Returns a matrix of integers indicating their column
#' number in a matrix-like object, or a factor of column labels.
#' @aliases col,DataMatrix-method
setMethod(
  f = "col",
  signature = c("DataMatrix"),
  definition = function(x, as.factor = FALSE) {
    size <- x@size
    idx <- index_by_column(size)
    if (as.factor) idx <- factor(idx, labels = x@column_names)
    matrix(data = idx, nrow = size[[1L]], ncol = size[[2L]])
  }
)

#' @export
#' @describeIn mutator Return the number of rows present in \code{x}.
#' @aliases nrow,DataMatrix-method
setMethod(
  f = "nrow",
  signature = c("DataMatrix"),
  definition = function(x) {
    x@size[[1L]]
  }
)

#' @export
#' @describeIn mutator Return the number of columns present in \code{x}.
#' @aliases ncol,DataMatrix-method
setMethod(
  f = "ncol",
  signature = c("DataMatrix"),
  definition = function(x) {
    x@size[[2L]]
  }
)

#' @export
#' @describeIn mutator Retrieve the dimnames of \code{x}.
#' @aliases dimnames,DataMatrix-method
setMethod(
  f = "dimnames",
  signature = c("DataMatrix"),
  definition = function(x) {
    list(rownames(x), colnames(x))
  }
)

#' @export
#' @describeIn mutator Set the dimnames of \code{x}.
#' @aliases dimnames<-,DataMatrix-method
setMethod(
  f = "dimnames<-",
  signature = c("DataMatrix"),
  definition = function(x, value) {
    rownames(x) <- value[[1L]]
    colnames(x) <- value[[2L]]
    methods::validObject(x)
    x
  }
)

#' @export
#' @describeIn mutator Retrieve the row names of \code{x}.
#' @aliases rownames,DataMatrix-method
setMethod(
  f = "rownames",
  signature = c("DataMatrix"),
  definition = function(x) {
    row_names <- x@row_names
    if (length(row_names) == 0) row_names <- NULL
    row_names
  }
)

#' @export
#' @describeIn mutator Set the row names of \code{x}.
#' @aliases rownames<-,DataMatrix-method
setMethod(
  f = "rownames<-",
  signature = c("DataMatrix"),
  definition = function(x, value) {
    x@row_names <- make.unique(as.character(value), sep = "_")
    methods::validObject(x)
    x
  }
)

#' @export
#' @describeIn mutator Retrieve the column names of \code{x}.
#' @aliases colnames,DataMatrix-method
setMethod(
  f = "colnames",
  signature = c("DataMatrix"),
  definition = function(x) {
    column_names <- x@column_names
    if (length(column_names) == 0) column_names <- NULL
    column_names
  }
)

#' @export
#' @describeIn mutator Set the column names of \code{x}.
#' @aliases colnames<-,DataMatrix-method
setMethod(
  f = "colnames<-",
  signature = c("DataMatrix"),
  definition = function(x, value) {
    x@column_names <- make.unique(as.character(value), sep = "_")
    methods::validObject(x)
    x
  }
)

# ====================================================================== Getters
#' @export
#' @rdname mutator
#' @aliases get_id,ANY-method
setMethod("get_id", "ANY", function(x) x@id)

#' @export
#' @rdname mutator
#' @aliases get_totals,AbundanceMatrix-method
setMethod("get_totals", "AbundanceMatrix", function(x) x@totals)

#' @export
#' @rdname mutator
#' @aliases get_method,SimilarityMatrix-method
setMethod("get_method", "SimilarityMatrix", function(x) x@method)

#' @export
#' @rdname mutator
#' @aliases get_units,StratigraphicMatrix-method
setMethod("get_units", "StratigraphicMatrix", function(x) x@row_names)

# ====================================================================== Setters
#' @export
#' @rdname mutator
#' @aliases set_id,ANY-method
setMethod(
  f = "set_id",
  signature = "ANY",
  definition = function(x) {
    x@id <- generate_uuid()
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutator
#' @aliases set_id,ANY-method
setMethod(
  f = "set_id<-",
  signature = "ANY",
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
