# CLASSES INITIALIZATION

# =================================================================== initialize
# setMethod(
#   f = "initialize",
#   signature = "DataMatrix",
#   definition = function(.Object, ..., values) {
#
#   }
# )

# ================================================================== constructor
#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))
  values <- as_integer(mtx)
  mtx[] <- seq_along(data)
  .CountMatrix(mtx, values = values)
}

#' @export
#' @rdname AbundanceMatrix-class
AbundanceMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))
  totals <- rowSums(mtx)
  values <- as.numeric(mtx / totals)
  values[is.nan(values)] <- 0 # Prevent division by zero
  mtx[] <- seq_along(data)
  .AbundanceMatrix(mtx, values = values, totals = totals)
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))
  values <- as.logical(mtx)
  mtx[] <- seq_along(data)
  .IncidenceMatrix(mtx, values = values)
}

SimilarityMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                             dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))
  if (!all(Reduce("==", dimnames(mtx)))) {
    varnames <- paste0("var", seq_len(nrow(mtx)))
    dimnames(mtx) <- list(varnames, varnames)
  }
  values <- as.numeric(mtx)
  mtx[] <- seq_along(data)
  .SimilarityMatrix(mtx, values = values, method = "unknown")
}

