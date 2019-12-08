# CLASSES INITIALIZATION

# ====================================================================== *Matrix
setMethod(
  f = "initialize",
  signature = "Matrix",
  definition = function(.Object, ..., id) {
    .Object@id <- if (missing(id)) generate_uuid() else id
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
setMethod(
  f = "initialize",
  signature = "StratigraphicMatrix",
  definition = function(.Object, ..., units) {
    .Object@units <- if (missing(units)) character(0) else units
    .Object <- methods::callNextMethod()
    methods::validObject(.Object)
    .Object
  }
)

#' Matrix constructor
#'
#' @inheritParams base::matrix
#' @param rows A \code{\link{logical}} scalar: is the number of rows
#'  unspecified?
#' @param cols A \code{\link{logical}} scalar: is the number of columns
#'  unspecified?
#' @return A \link{\code{matrix}}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
build_matrix <- function(data, nrow, ncol, byrow, dimnames,
                        rows = FALSE, cols = FALSE) {
  k <- length(data)
  if (rows) nrow <- k / ncol
  if (cols) ncol <- k / nrow

  if (is.null(dimnames))
    dimnames <- list(NULL, NULL)
  if (is.null(dimnames[[1]]) && nrow > 0)
    dimnames[[1]] <- seq_len(nrow)
  if (is.null(dimnames[[2]]) && ncol > 0)
    dimnames[[2]] <- paste0("V", seq_len(ncol))

  M <- matrix(data, nrow, ncol, byrow, dimnames)
  return(M)
}

#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL, ...) {
  M <- build_matrix(data, nrow, ncol, byrow, dimnames,
                    missing(nrow), missing(ncol))
  .CountMatrix(M, ...)
}

# @rdname AbundanceMatrix-class
AbundanceMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  M <- build_matrix(data, nrow, ncol, byrow, dimnames,
                    missing(nrow), missing(ncol))
  totals <- rowSums(M)
  M <- M / totals
  .AbundanceMatrix(M, ..., totals = totals)
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  data <- as.logical(data)
  M <- build_matrix(data, nrow, ncol, byrow, dimnames,
                    missing(nrow), missing(ncol))
  .IncidenceMatrix(M, ...)
}
