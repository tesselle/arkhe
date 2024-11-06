# GET

#' @export
#' @rdname get
#' @aliases get_columns,ANY-method
setMethod(
  f = "get_columns",
  signature = c(x = "ANY"),
  definition = function(x, select = NULL, names = NULL, ...) {
    i <- seek_columns(x, select = select, names = names, ...)
    x[, i, drop = FALSE]
  }
)

#' @export
#' @rdname get
#' @aliases get_rows,ANY-method
setMethod(
  f = "get_rows",
  signature = c(x = "ANY"),
  definition = function(x, select = NULL, names = NULL, ...) {
    i <- seek_rows(x, select = select, names = names, ...)
    x[i, , drop = FALSE]
  }
)
