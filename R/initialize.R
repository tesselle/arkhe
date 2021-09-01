# INITIALIZE
#' @include AllClasses.R
NULL

#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  mtx <- make_matrix(
    data = data,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )
  methods::as(mtx, "CountMatrix")
}

#' @export
#' @rdname CompositionMatrix-class
CompositionMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                              dimnames = NULL) {
  mtx <- make_matrix(
    data = data,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )
  methods::as(mtx, "CompositionMatrix")
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  mtx <- make_matrix(
    data = data,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    dimnames = dimnames,
    missing(nrow),
    missing(ncol)
  )
  methods::as(mtx, "IncidenceMatrix")
}

# Matrix =======================================================================
#' Build a Matrix
#'
#' Creates a matrix from the given set of values.
#' @return A [`matrix`].
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
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
