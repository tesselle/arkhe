# HELPERS

#' Helpers
#'
#' \code{compact} removes elements from a list or vector.
#' \code{detect} finds values in a list or vector according to a given
#' predicate.
#' \code{count} counts values in a list or vector according to a given
#' predicate.
#' \code{extract} extracts a string form another string based on a pattern.
#'
#' %o% allows for function composition.
#' %||% allows to define a default value.
#' @param x,y An object.
#' @param f,g A \code{\link{function}}. In \code{compact}, \code{detect}
#'  and \code{count} \code{f} must be a \code{\link{logical}} predicate.
#' @param pattern A \code{\link{character}} string containing a regular
#'  expression.
#' @references
#'  Wickham, H. (2014). \emph{Advanced R}. London: Chapman & Hall. The R Series.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) || length(x) != 0) x else y
}
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
compact <- function(f, x) {
  Filter(Negate(f), x)
}
detect <- function(f, x) {
  vapply(x, f, logical(1))
}
count <- function(f, x) {
  sum(detect(f, x))
}
extract <- function(x, pattern) {
  regmatches(x, regexpr(pattern, x))
}
