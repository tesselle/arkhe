# CLASSES INITIALIZATION

# ====================================================================== *Matrix
Matrix <- function(..., id, dates) {
  id <- if (missing(id)) generate_uuid(NULL) else id
  dates <- if (missing(dates)) list() else dates
  throw_message("codex_message_class",
                "<Matrix> instance initialized.\n")
  .Matrix(..., id = id, dates = dates)
}
NumericMatrix <- function(...) {
  throw_message("codex_message_class",
                "<NumericMatrix> instance initialized.\n")
  .NumericMatrix(Matrix(...))
}
LogicalMatrix <- function(...) {
  throw_message("codex_message_class",
                "<LogicalMatrix> instance initialized.\n")
  .LogicalMatrix(Matrix(...))
}
OccurrenceMatrix <- function(...) {
  throw_message("codex_message_class",
                "<OccurrenceMatrix> instance initialized.\n")
  .OccurrenceMatrix(NumericMatrix(...))
}
SimilarityMatrix <- function(..., method = "unknown") {
  throw_message("codex_message_class",
                "<SimilarityMatrix> instance initialized.\n")
  .SimilarityMatrix(NumericMatrix(...), method = method)
}
StratigraphicMatrix <- function(...) {
  throw_message("codex_message_class",
                "<StratigraphicMatrix> instance initialized.\n")
  .StratigraphicMatrix(LogicalMatrix(...))
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
#' @keywords internal
#' @noRd
buildMatrix <- function(data, nrow, ncol, byrow, dimnames,
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
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  throw_message("codex_message_class",
                "<CountMatrix> instance initialized.\n")
  .CountMatrix(NumericMatrix(M), ...)
}

#' @export
#' @rdname FrequencyMatrix-class
FrequencyMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  totals <- rowSums(M)
  M <- M / totals
  throw_message("codex_message_class",
                "<FrequencyMatrix> instance initialized.\n")
  .FrequencyMatrix(NumericMatrix(M), ..., totals = totals)
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  data <- as.logical(data)
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  throw_message("codex_message_class",
                "<IncidenceMatrix> instance initialized.\n")
  .IncidenceMatrix(LogicalMatrix(M), ...)
}
