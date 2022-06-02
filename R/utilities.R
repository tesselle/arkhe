# HELPERS

# Helpers ======================================================================
#' Helpers
#'
# * `compact()` removes elements from a list or vector.
# * `detect()` finds values in a list or vector according to a given predicate.
# * `count()` counts values in a list or vector according to a given predicate.
#' * `extract()` extracts a string form another string based on a pattern.
#' * `\%o\%` allows for function composition.
#' * `\%||\%` allows to define a default value.
#' @param x,y An object.
#' @param f,g A [`function`]. In `compact()`, `detect()` and `count()` `f` must
#'  be a [`logical`] predicate.
#' @param pattern A [`character`] string containing a regular expression.
#' @references
#'  Wickham, H. (2014). *Advanced R*. London: Chapman & Hall. The R Series.
#' @family utilities
#' @keywords internal utilities
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
# compact <- function(f, x) {
#   Filter(Negate(f), x)
# }
# detect <- function(f, x) {
#   vapply(x, f, logical(1))
# }
# count <- function(f, x) {
#   sum(detect(f, x))
# }
extract <- function(x, pattern) {
  regmatches(x, regexpr(pattern, x))
}
