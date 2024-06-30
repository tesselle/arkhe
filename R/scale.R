# DATA TRANSFORMATION
NULL

#' Rescale Continuous Vector (minimum, maximum)
#'
#' Rescales continuous vector to have specified minimum and maximum.
#' @param x A [`numeric`] vector.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @return A [`numeric`] vector.
#' @note For internal use only.
#' @family scales
#' @export
scale_range <- function(x, to = c(0, 1), from = range(x, finite = TRUE)) {
  if (.is_zero(to) || .is_zero(from)) return(ifelse(is.na(x), NA, mean(to)))
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

#' Rescale Continuous Vector (minimum, midpoint, maximum)
#'
#' Rescales continuous vector to have specified minimum, midpoint and maximum.
#' @param x A [`numeric`] vector.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @param midpoint A length-one [`numeric`] vector specifying the mid-point of
#'  input range.
#' @return A [`numeric`] vector.
#' @note For internal use only.
#' @family scales
#' @export
scale_midpoint <- function(x, to = c(0, 1), from = range(x, finite = TRUE), midpoint = 0) {
  if (.is_zero(to) || .is_zero(from)) return(ifelse(is.na(x), NA, mean(to)))
  extent <- 2 * max(abs(from - midpoint))
  (x - midpoint) / extent * diff(to) + mean(to)
}

.is_zero <- function(x, tolerance = sqrt(.Machine$double.eps)) {
  diff(range(x)) <= tolerance
}
