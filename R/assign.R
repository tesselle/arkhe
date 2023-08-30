# DATA CLEANING: ASSIGN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname assign
#' @aliases assign_rownames,data.frame-method
setMethod(
  f = "assign_rownames",
  signature = c(x = "data.frame"),
  definition = function(x, column, remove = TRUE) {
    ## Validation
    assert_length(column, 1)
    if ((column > ncol(x)) | (column < 1)) return(x)

    y <- x
    rownames(y) <- y[, column]
    if (remove) {
      y <- y[, -column, drop = FALSE]
    }
    y
  }
)

#' @export
#' @rdname assign
#' @aliases assign_colnames,data.frame-method
setMethod(
  f = "assign_colnames",
  signature = c(x = "data.frame"),
  definition = function(x, row, remove = TRUE) {
    ## Validation
    assert_length(row, 1)
    if ((row > nrow(x)) | (row < 1)) return(x)

    y <- x
    colnames(y) <- y[row, ]
    if (remove) {
      y <- y[-row, , drop = FALSE]
    }
    y
  }
)
