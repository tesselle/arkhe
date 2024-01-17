# DATA TRANSFORMATION
NULL

#' Rescale Continuous Vector
#'
#' Rescales continuous vector to have specified minimum and maximum.
#' @param x A [`numeric`] vector.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @note For internal use only.
#' @return A [`numeric`] vector.
#' @family data transformation tools
#' @export
scale_range <- function(x, to = c(0, 1), from = range(x, finite = TRUE)) {
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}
