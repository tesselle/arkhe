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
needs <- function(...) {
  # .Deprecated("assert_package()", old = "needs()")
  assert_package(...)
}

#' @export
#' @rdname arkhe-deprecated
assert_dimensions <- function(...) {
  # .Deprecated("assert_dim()", old = "assert_dimensions()")
  assert_dim(...)
}

#' @export
#' @rdname arkhe-deprecated
compact_cols <- function(...) {
  # .Deprecated("compact_columns()", old = "compact_cols()")
  compact_columns(...)
}

#' @export
#' @rdname arkhe-deprecated
discard_cols <- function(...) {
  # .Deprecated("discard_columns()", old = "discard_cols()")
  discard_columns(...)
}

#' @export
#' @rdname arkhe-deprecated
keep_cols <- function(...) {
  # .Deprecated("keep_columns()", old = "keep_cols()")
  keep_columns(...)
}
