# DATA CLEANING
#' @include AllClasses.R AllGenerics.R
NULL

# Replace ======================================================================
## Missing values --------------------------------------------------------------
#' @export
#' @rdname clean
#' @aliases replace_NA,matrix-method
setMethod(
  f = "replace_NA",
  signature = signature(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.na(x)] <- value
    x
  }
)

# Remove =======================================================================
## Missing values --------------------------------------------------------------
#' @export
#' @rdname clean
#' @aliases remove_NA,matrix-method
setMethod(
  f = "remove_NA",
  signature = signature(x = "matrix"),
  definition = function(x, margin = 1, finite = TRUE) {
    index <- !detect_missing(x, margin = margin, finite = finite)
    if (margin == 1) x <- x[index, , drop = FALSE]
    if (margin == 2) x <- x[, index, drop = FALSE]
    x
  }
)

## Zeros -----------------------------------------------------------------------
#' @export
#' @rdname clean
#' @aliases remove_zero,matrix-method
setMethod(
  f = "remove_zero",
  signature = signature(x = "matrix"),
  definition = function(x, margin = 1) {
    index <- !detect_zero(x, margin = margin)
    if (margin == 1) x <- x[index, , drop = FALSE]
    if (margin == 2) x <- x[, index, drop = FALSE]
    x
  }
)

## Empty -----------------------------------------------------------------------
#' @export
#' @rdname clean
#' @aliases remove_empty,matrix-method
setMethod(
  f = "remove_empty",
  signature = signature(x = "matrix"),
  definition = function(x, margin = 1) {
    index <- !detect_empty(x, margin = margin)
    if (margin == 1) x <- x[index, , drop = FALSE]
    if (margin == 2) x <- x[, index, drop = FALSE]
    x
  }
)

# Detect =======================================================================
#' Data Cleaning
#'
#' @param x An object. It will be coerced to a [`matrix`] as by
#'  [as.matrix()].
#' @param margin A vector giving the subscripts which the function will be
#'  applied over (see [apply()]).
#' @param finite A [`logical`] scalar: should non-[`finite`] values also be
#'  removed?
#' @param f A predicate [`function`].
#' @param type A [`character`] vector.
#' @return
#'  `detect_missing()`, `detect_zero()` and `detect_any()` return a [`logical`]
#'  vector.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
detect_missing <- function(x, margin = 1, finite = FALSE) {
  is_missing <- if (finite) function(x) !is.finite(x) else is.na
  detect_any(x, f = is_missing, margin = margin, type = "NAs")
}
detect_zero <- function(x, margin = 1) {
  is_zero <- function(x) if (is.numeric(x)) x == 0 else rep(FALSE, length(x))
  detect_any(x, f = is_zero, margin = margin, type = "zeros")
}
detect_empty <- function(x, margin = 1) {
  is_empty <- function(x) if (is.numeric(x)) sum(x, na.rm = TRUE) == 0 else FALSE
  detect_any(x, f = is_empty, margin = margin, type = "empty")
}
detect_any <- function(x, f, margin = 1, type = "generic") {
  count <- apply(X = x, MARGIN = margin, FUN = function(x, f) sum(f(x)), f = f)
  index <- count > 0

  n <- sum(index)
  if (n > 0 & getOption("arkhe.verbose")) {
    parts <- dimnames(x)[[margin]]
    if (is.null(parts)) parts <- seq_len(dim(x)[[margin]])
    mar <- ifelse(
      margin == 1,
      ngettext(n, "row", "rows"),
      ngettext(n, "column", "columns")
    )
    k <- paste0(sprintf("* %s (%d)", parts[index], count[index]),
                collapse = "\n")
    message(sprintf("Removed %d %s (%s):\n%s", n, mar, type, k))
  }

  index
}
