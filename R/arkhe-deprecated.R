# DEPRECATED
#' @include assert.R
NULL

#' Deprecated Functions in arkhe
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name arkhe-deprecated
#' @keywords internal
NULL

#' @export
#' @rdname arkhe-deprecated
assert_dimensions <- function(x, expected) {
  # .Deprecated("assert_dim()", old = "assert_dimensions()")
  assert_dim(x, expected)
}
