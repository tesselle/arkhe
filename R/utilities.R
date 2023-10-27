# UTILITIES
#' @include AllGenerics.R
NULL

#' Default value for NULL
#'
#' Replaces `NULL` with a default value.
#' @param x,y An object.
#' @return If `x` is `NULL`, returns `y`; otherwise returns `x`.
#' @family utilities
#' @keywords utilities
#' @name null
#' @rdname null
#' @export
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

#' Concatenate
#'
#' Concatenates character vectors.
#' @param x,y A [`character`] vector.
#' @return A [`character`] vector.
#' @family utilities
#' @keywords utilities
#' @name concat
#' @rdname concat
#' @export
`%+%` <- function (x, y) {
  stopifnot(is.character(x), is.character(y))
  stopifnot(length(x) == length(y) || length(x) == 1 || length(y) == 1)

  if (length(x) == 0 && length(y) == 0) paste0(x, y)
  else if (length(x) == 0) x
  else if (length(y) == 0) y
  else paste0(x, y)
}
