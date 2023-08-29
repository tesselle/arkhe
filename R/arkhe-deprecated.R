#' Deprecated Functions in arkhe
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name arkhe-deprecated
#' @keywords internal
NULL

#' @rdname arkhe-deprecated
#' @aliases confidence-method
setGeneric(
  name = "confidence",
  def = function(object, ...) standardGeneric("confidence")
)

#' @export
#' @rdname arkhe-deprecated
#' @aliases confidence,numeric-method
setMethod(
  f = "confidence",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95, type = c("student", "normal")) {
    .Deprecated("confidence_mean()", old = "confidence()")
    confidence_mean(object, level = level, type = type)
  }
)
