# CLASSES INITIALIZATION

# Constructors =================================================================
#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))

  .CountMatrix(
    size = dim(mtx),
    row_names = rownames(mtx),
    column_names = colnames(mtx),
    values = as_integer(mtx)
  )
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

  .AbundanceMatrix(
    size = dim(mtx),
    row_names = rownames(mtx),
    column_names = colnames(mtx),
    values = values,
    totals = totals
  )
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))

  .IncidenceMatrix(
    size = dim(mtx),
    row_names = rownames(mtx),
    column_names = colnames(mtx),
    values = as.logical(mtx)
  )
}

SimilarityMatrix <- function(data = 1, nrow = 1, ncol = 1, byrow = FALSE,
                             dimnames = NULL) {
  mtx <- make_matrix(data, nrow, ncol, byrow, dimnames,
                     missing(nrow), missing(ncol))
  dm <- dimnames(mtx)
  if (is.null(dm) || any(detect(is.null, dm)) || !all(Reduce("==", dm))) {
    varnames <- paste0("var", seq_len(nrow(mtx)))
    dimnames(mtx) <- list(varnames, varnames)
  }

  .SimilarityMatrix(
    size = dim(mtx),
    row_names = rownames(mtx),
    column_names = colnames(mtx),
    values = as.numeric(mtx),
    method = "unknown"
  )
}

# Helpers ======================================================================
#' Build a Matrix
#'
#' Creates a matrix from the given set of values.
#' @return A \code{\link{matrix}}.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
make_names <- function(x, n = 0, prefix = "var") {
  if (is.null(x)) {
    x <- if (n > 0) paste0(prefix, seq_len(n)) else character(0)
  } else {
    x <- make.unique(as.character(x), sep = "_")
  }
  x
}
make_dimnames <- function(x) {
  list(
    make_names(dimnames(x)[[1L]], nrow(x), "row"),
    make_names(dimnames(x)[[2L]], ncol(x), "col")
  )
}
make_matrix <- function(data, nrow, ncol, byrow, dimnames, row, col) {
  n <- length(data)
  if (col) ncol <- n / nrow
  if (row) nrow <- n / ncol
  if (byrow) {
    dim(data) <- c(ncol, nrow)
    data <- t(data)
  } else {
    dim(data) <- c(nrow, ncol)
  }

  # Make dimnames
  row_names <- make_names(dimnames[[1L]], nrow, "row")
  column_names <- make_names(dimnames[[2L]], ncol, "col")
  dimnames(data) <- list(row_names, column_names)

  data
}
