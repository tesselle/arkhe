# DATA CLEANING
#' @include AllClasses.R AllGenerics.R
NULL

# Replace ======================================================================
## Missing values --------------------------------------------------------------
#' @export
#' @rdname replace
#' @aliases replace_NA,matrix-method
setMethod(
  f = "replace_NA",
  signature = signature(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.na(x)] <- value
    x
  }
)

## Infinite values -------------------------------------------------------------
#' @export
#' @rdname replace
#' @aliases replace_Inf,matrix-method
setMethod(
  f = "replace_Inf",
  signature = signature(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.infinite(x)] <- value
    x
  }
)

## Zeros -----------------------------------------------------------------------
#' @export
#' @rdname replace
#' @aliases replace_zero,matrix-method
setMethod(
  f = "replace_zero",
  signature = signature(x = "matrix"),
  definition = function(x, value) {
    x[is_zero(x)] <- value
    x
  }
)

# Remove =======================================================================
## Missing values --------------------------------------------------------------
#' @export
#' @rdname remove
#' @aliases remove_NA,matrix-method
setMethod(
  f = "remove_NA",
  signature = signature(x = "matrix"),
  definition = function(x, margin = 1) {
    index <- !detect_missing(x, margin = margin)
    if (margin == 1) x <- x[index, , drop = FALSE]
    if (margin == 2) x <- x[, index, drop = FALSE]
    x
  }
)

## Infinite values -------------------------------------------------------------
#' @export
#' @rdname remove
#' @aliases remove_Inf,matrix-method
setMethod(
  f = "remove_Inf",
  signature = signature(x = "matrix"),
  definition = function(x, margin = 1) {
    index <- !detect_infinite(x, margin = margin)
    if (margin == 1) x <- x[index, , drop = FALSE]
    if (margin == 2) x <- x[, index, drop = FALSE]
    x
  }
)

## Zeros -----------------------------------------------------------------------
#' @export
#' @rdname remove
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
#' @rdname remove
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
#' @param x An object. It will be coerced to a [`matrix`] as by [as.matrix()].
#' @param margin A vector giving the subscripts which the function will be
#'  applied over (see [apply()]).
#' @param f A predicate [`function`].
#' @param type A [`character`] vector.
#' @return
#'  A [`logical`] vector.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
detect_missing <- function(x, margin = 1) {
  detect_any(x, f = is.na, margin = margin)
}
detect_infinite <- function(x, margin = 1) {
  detect_any(x, f = is.infinite, margin = margin)
}
detect_zero <- function(x, margin = 1) {
  detect_any(x, f = is_zero, margin = margin)
}
detect_empty <- function(x, margin = 1, na.rm = TRUE) {
  all_zero <- function(x) {
    if (is_numeric(x)) all(x == 0, na.rm = na.rm)
    else if (is_character(x)) all(x == "", na.rm = na.rm)
    else stop("Don't know what to do...", call. = FALSE)
  }
  detect_any(x, f = all_zero, margin = margin)
}
detect_any <- function(x, f, margin = 1, ...) {
  count <- apply(
    X = x,
    MARGIN = margin,
    FUN = function(x, f, ...) sum(f(x, ...)),
    f = f, ...
  )
  index <- count > 0

  n <- sum(index)
  if (n > 0 & getOption("arkhe.verbose")) {
    parts <- dimnames(x)[[margin]]
    if (is.null(parts)) parts <- seq_len(dim(x)[[margin]])
    mar <- ifelse(
      margin == 1,
      ngettext(n, "row was", "rows were"),
      ngettext(n, "column was", "columns were")
    )
    k <- paste0(sprintf("* %s (n=%d)", parts[index], count[index]),
                collapse = "\n")
    message(sprintf("%d %s removed:\n%s", n, mar, k))
  }

  index
}
