#' Deprecated Functions in arkhe
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name arkhe-deprecated
#' @keywords internal
NULL

#' @rdname arkhe-deprecated
#' @aliases wide_to_long-method
setGeneric(
  name = "wide_to_long",
  def = function(from, ...) standardGeneric("wide_to_long"),
  valueClass = "data.frame"
)

#' @rdname arkhe-deprecated
#' @aliases to_long-method
setGeneric(
  name = "to_long",
  def = function(from, ...) standardGeneric("to_long"),
  valueClass = "data.frame"
)

#' @export
#' @rdname arkhe-deprecated
#' @aliases wide_to_long,matrix-method
setMethod(
  f = "wide_to_long",
  signature = c(from = "matrix"),
  definition = function(from, factor = FALSE, reverse = FALSE) {
    .Deprecated(old = "wide_to_long")
    x <- data.frame(
      row = as.vector(row(from, as.factor = factor)),
      column = as.vector(col(from, as.factor = factor)),
      value = as.vector(from),
      stringsAsFactors = FALSE
    )
    if (factor) {
      x$row <- as_factor(x$row, reverse = reverse)
      x$column <- as_factor(x$column, reverse = reverse)
    }
    x
  }
)

#' @export
#' @rdname arkhe-deprecated
#' @aliases to_long,matrix-method
setMethod(
  f = "to_long",
  signature = c(from = "matrix"),
  definition = function(from, factor = FALSE, reverse = FALSE) {
    wide_to_long(from, factor = factor, reverse = reverse)
  }
)

#' Factors
#'
#' @param x A vector to be coerced.
#' @param reverse A [`logical`] scalar: should the order of factor
#'  levels be reversed? Useful for plotting.
#' @details
#'  Encodes a vector as a factor without sorting it (preserves original
#'  ordering or reverse it if `reverse` is `TRUE`).
#' @return An [`factor`] object.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
as_factor <- function(x, reverse = FALSE) {
  lvl <- unique(x)
  if (reverse) {
    lvl <- rev(lvl)
  }
  factor(x, levels = lvl)
}
