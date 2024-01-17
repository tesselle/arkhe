# GET

#' @export
#' @rdname get
#' @aliases get_columns,data.frame-method
setMethod(
  f = "get_columns",
  signature = c(x = "data.frame"),
  definition = function(x, select = NULL, ...) {
    i <- seek_columns(x, select = select, ...)
    x[, i, drop = FALSE]
  }
)

#' @export
#' @rdname get
#' @aliases get_rows,data.frame-method
setMethod(
  f = "get_rows",
  signature = c(x = "data.frame"),
  definition = function(x, select = NULL, ...) {
    i <- seek_rows(x, select = select, ...)
    x[i, , drop = FALSE]
  }
)
