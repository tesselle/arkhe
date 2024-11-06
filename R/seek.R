# SEEK

seek <- function(x, margin = 2, select = NULL, names = NULL, ...) {
  assert_filled(x)
  assert_length(margin, 1)
  assert_type(names, "character", allow_null = TRUE)

  dm <- dim(x)[[margin]]
  nm <- dimnames(x)[[margin]]
  if (is.null(nm)) return(NULL)

  if (is.null(select)) {
    if (!is.null(names)) {
      select <- function(i) match(names, i)
    } else {
      return(seq_len(dm))
    }
  }

  assert_function(select)
  i <- select(nm, ...)

  if (is.logical(i)) i <- which(i)
  if (length(i) == 0 || all(is.na(i))) i <- NULL
  if (!is.null(i)) assert_type(i, "integer")
  i
}

#' @export
#' @rdname seek
#' @aliases seek_rows,data.frame-method
setMethod(
  f = "seek_rows",
  signature = c(x = "data.frame"),
  definition = function(x, select = NULL, names = NULL, ...) {
    # assert_rownames(x)
    seek(x, margin = 1, select = select, names = names, ...)
  }
)

#' @export
#' @rdname seek
#' @aliases seek_rows,matrix-method
setMethod(
  f = "seek_rows",
  signature = c(x = "matrix"),
  definition = function(x, select = NULL, names = NULL, ...) {
    # assert_rownames(x)
    seek(x, margin = 1, select = select, names = names, ...)
  }
)

#' @export
#' @rdname seek
#' @aliases seek_columns,data.frame-method
setMethod(
  f = "seek_columns",
  signature = c(x = "data.frame"),
  definition = function(x, select = NULL, names = NULL, ...) {
    # assert_colnames(x)
    seek(x, margin = 2, select = select, names = names, ...)
  }
)

#' @export
#' @rdname seek
#' @aliases seek_columns,matrix-method
setMethod(
  f = "seek_columns",
  signature = c(x = "matrix"),
  definition = function(x, select = NULL, names = NULL, ...) {
    # assert_colnames(x)
    seek(x, margin = 2, select = select, names = names, ...)
  }
)
