# CLASSES INITIALIZATION

# ==================================================================== SpaceTime
SpaceTime <- function(
  dates = matrix(0, 0, 2, dimnames = list(NULL, c("value", "error"))),
  coordinates = matrix(0, 0, 3, dimnames = list(NULL, c("X", "Y", "Z"))),
  epsg = 0, ...
) {
  throw_message_class("SpaceTime")
  colnames(coordinates) <- toupper(colnames(coordinates))
  .SpaceTime(
    dates = dates,
    coordinates = coordinates,
    epsg = as.integer(epsg),
    ...
  )
}

# ====================================================================== *Matrix
Matrix <- function(...) {
  throw_message_class("Matrix")
  .Matrix(..., id = generate_uuid(seed = NULL))
}
NumericMatrix <- function(data = matrix(0, 0, 0), ...) {
  throw_message_class("NumericMatrix")
  .NumericMatrix(Matrix(data), ...)
}
OccurrenceMatrix <- function(data = matrix(0, 0, 0), ...) {
  throw_message_class("OccurrenceMatrix")
  .OccurrenceMatrix(NumericMatrix(data), ...)
}
SimilarityMatrix <- function(data = matrix(0, 0, 0), method = "unknown", ...) {
  throw_message_class("SimilarityMatrix")
  .SimilarityMatrix(NumericMatrix(data), method = as.character(method), ...)
}
LogicalMatrix <- function(data = matrix(FALSE, 0, 0), ...) {
  throw_message_class("LogicalMatrix")
  .LogicalMatrix(Matrix(data), ...)
}
StratigraphicMatrix <- function(data = matrix(FALSE, 0, 0), ...) {
  throw_message_class("LogicalMatrix")
  .StratigraphicMatrix(LogicalMatrix(data), ...)
}

# -------------------------------------------------------------- AbundanceMatrix
#' Matrix constructor
#'
#' @inheritParams base::matrix
#' @param rows A \code{\link{logical}} scalar: is the number of rows
#'  unspecified?
#' @param cols A \code{\link{logical}} scalar: is the number of columns
#'  unspecified?
#' @return A \link{\code{matrix}}.
#' @author N. Frerebeau
#' @name matrix-constructors
#' @keywords internal
#' @noRd
buildMatrix <- function(data, nrow, ncol, byrow, dimnames,
                        rows = FALSE, cols = FALSE) {
  k <- length(data)
  if (rows) nrow <- k / ncol
  if (cols) ncol <- k / nrow
  if (is.null(dimnames)) {
    dimnames <- list(seq_len(nrow), paste("V", seq_len(ncol), sep = ""))
  } else {
    if (is.null(dimnames[[1]])) dimnames[[1]] <- seq_len(nrow)
    if (is.null(dimnames[[2]])) dimnames[[2]] <- paste0("V", seq_len(ncol))
  }
  M <- matrix(data, nrow, ncol, byrow, dimnames)
  return(M)
}

#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL, ...) {
  throw_message_class("CountMatrix")
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  .CountMatrix(NumericMatrix(M), ...)
}

#' @export
#' @rdname FrequencyMatrix-class
FrequencyMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  throw_message_class("FrequencyMatrix")
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  totals <- rowSums(M)
  M <- M / totals
  .FrequencyMatrix(NumericMatrix(M), totals = totals, ...)
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  throw_message_class("IncidenceMatrix")
  data <- as.logical(data)
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  .IncidenceMatrix(LogicalMatrix(M), ...)
}
