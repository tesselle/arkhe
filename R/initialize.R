# CLASSES INITIALIZATION

# Constructors =================================================================
#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  mtx <- make_matrix(
    data = as_integer(data),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )

  .CountMatrix(mtx)
}

#' @export
#' @rdname AbundanceMatrix-class
AbundanceMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(
    data = as.numeric(data),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )
  totals <- rowSums(mtx)
  mtx <- mtx / totals
  mtx[is.nan(mtx)] <- 0 # Prevent division by zero

  .AbundanceMatrix(mtx, totals = totals)
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(
    data = as.logical(data),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )

  .IncidenceMatrix(mtx)
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

  if (nrow == 0 & ncol == 0) {
    mtx <- matrix(data = data, nrow = 0, ncol = 0)
    return(mtx)
  }

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
