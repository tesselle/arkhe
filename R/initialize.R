# CLASSES INITIALIZATION

# =================================================================== initialize
setMethod(
  f = "initialize",
  signature = "GenericMatrix",
  definition = function(.Object, ..., id, size, row_names, column_names,
                        dates, coordinates) {

    .Object@id <- if (missing(id)) generate_uuid() else id
    .Object@size <- if (missing(size)) c(0L, 0L) else as.integer(size)
    .Object@row_names <- if (missing(row_names)) character(0) else as.character(row_names)
    .Object@column_names <- if (missing(column_names)) character(0) else as.character(column_names)
    .Object@dates <- if (missing(dates)) vector("list", 0L) else dates
    .Object@coordinates <- if (missing(coordinates)) vector("list", 0L) else coordinates

    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)
setMethod(
  f = "initialize",
  signature = "AbundanceMatrix",
  definition = function(.Object, ..., totals) {
    .Object <- methods::callNextMethod()
    if (missing(totals)) {
      size <- .Object@size
      nrows <- size[[1L]]
      ncols <- size[[2L]]
      index <- rep(seq_len(nrows), times = ncols)
      .Object@totals <- vapply(
        X = split(x = .Object@data, f = index),
        FUN = sum,
        FUN.VALUE = integer(1)
      )
    } else {
      .Object@totals <- totals
    }
    methods::validObject(.Object)
    .Object
  }
)
setMethod(
  f = "initialize",
  signature = "OccurrenceMatrix",
  definition = function(.Object, ..., n) {
    .Object@n <- if (missing(n)) 0L else n
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)
setMethod(
  f = "initialize",
  signature = "SimilarityMatrix",
  definition = function(.Object, ..., method) {
    .Object@method <- if (missing(method)) "unknown" else method
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

# ================================================================== constructor
#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  n <- length(data)
  if (missing(ncol)) ncol <- n / nrow
  if (missing(nrow)) nrow <- n / ncol
  mtx <- matrix(data = data, nrow = nrow, ncol = ncol, byrow = byrow,
                dimnames = dimnames)
  size <- dim(mtx)

  .CountMatrix(
    data = as_integer(mtx),
    size = size,
    row_names = make_names(dimnames[[1L]], size[[1L]], "row"),
    column_names = make_names(dimnames[[2L]], size[[2L]], "col")
  )
}

AbundanceMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, totals = NULL) {
  n <- length(data)
  if (missing(ncol)) ncol <- n / nrow
  if (missing(nrow)) nrow <- n / ncol
  mtx <- matrix(data = data, nrow = nrow, ncol = ncol, byrow = byrow,
                dimnames = dimnames)
  size <- dim(mtx)
  totals <- if (is.null(totals)) rowSums(mtx) else totals

  .AbundanceMatrix(
    data = as.numeric(mtx),
    totals = as.numeric(totals),
    size = size,
    row_names = make_names(dimnames[[1L]], size[[1L]], "row"),
    column_names = make_names(dimnames[[2L]], size[[2L]], "col")
  )
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  n <- length(data)
  if (missing(ncol)) ncol <- n / nrow
  if (missing(nrow)) nrow <- n / ncol
  mtx <- matrix(data = data, nrow = nrow, ncol = ncol, byrow = byrow,
                dimnames = dimnames)
  size <- dim(mtx)

  .IncidenceMatrix(
    data = as.logical(data),
    size = size,
    row_names = make_names(dimnames[[1L]], size[[1L]], "row"),
    column_names = make_names(dimnames[[2L]], size[[2L]], "col")
  )
}
