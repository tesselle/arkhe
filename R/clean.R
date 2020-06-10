# DATA CLEANING
#' @include AllClasses.R AllGenerics.R
NULL

# Missing values ===============================================================
#' @export
#' @rdname clean
#' @aliases remove_NA,matrix-method
setMethod(
  f = "remove_NA",
  signature = signature(object = "matrix"),
  definition = function(object, margin = 1, finite = TRUE) {
    index <- detect_missing(object, margin = margin, finite = finite)
    if (margin == 1) x <- object[!index, , drop = FALSE]
    if (margin == 2) x <- object[, !index, drop = FALSE]
    x
  }
)

#' @export
#' @rdname clean
#' @aliases remove_NA,data.frame-method
setMethod(
  f = "remove_NA",
  signature = signature(object = "data.frame"),
  definition = function(object, margin = 1, finite = TRUE) {
    index <- detect_missing(object, margin = margin, finite = finite)
    if (margin == 1) x <- object[!index, , drop = FALSE]
    if (margin == 2) x <- object[, !index, drop = FALSE]
    x
  }
)

# Zeros ========================================================================
#' @export
#' @rdname clean
#' @aliases remove_zero,matrix-method
setMethod(
  f = "remove_zero",
  signature = signature(object = "matrix"),
  definition = function(object, margin = 1) {
    index <- detect_zero(object, margin = margin)
    if (margin == 1) x <- object[!index, , drop = FALSE]
    if (margin == 2) x <- object[, !index, drop = FALSE]
    x
  }
)

#' @export
#' @rdname clean
#' @aliases remove_zero,data.frame-method
setMethod(
  f = "remove_zero",
  signature = signature(object = "data.frame"),
  definition = function(object, margin = 1) {
    index <- detect_zero(object, margin = margin)
    if (margin == 1) x <- object[!index, , drop = FALSE]
    if (margin == 2) x <- object[, !index, drop = FALSE]
    x
  }
)

# Detect (internal) ============================================================
#' Data Cleaning
#'
#' @param x An object. It will be coerced to a \code{matrix} as by
#'  \code{\link{as.matrix}}.
#' @param margin A vector giving the subscripts which the function will be
#'  applied over (see \code{\link{apply}}).
#' @param finite A \code{\link{logical}} scalar: should non-\code{\link{finite}}
#' values also be removed?
#' @param f A predicate \code{\link{function}}.
#' @param type A \code{\link{character}} vector.
#' @return
#'  \code{detect_missing}, \code{detect_zero} and \code{detect_any} return
#'  an \code{\link{integer}} vector of indices.
#'
#'  \code{clean} returns a \code{\link{list}} with the following elements:
#'  \describe{
#'   \item{data}{A \code{\link{numeric}} matrix.}
#'   \item{index}{An \code{\link{integer}} vector giving the indiced of the rows
#'   that were removed, if any.}
#'  }
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
detect_missing <- function(x, margin = 1, finite = FALSE) {
  if (finite) {
    is_missing <- function(x) is.na(x) | !is.finite(x)
  } else {
    is_missing <- is.na
  }
  detect_any(x, f = is_missing, margin = margin, type = "NAs")
}
detect_zero <- function(x, margin = 1) {
  is_zero <- function(x) {
    if (is.numeric(x)) x == 0
    else rep(FALSE, length(x))
  }
  detect_any(x, f = is_zero, margin = margin, type = "zeros")
}
detect_any <- function(x, f, margin = 1, type = "generic") {
  if (is.list(x)) {
    count <- vapply(X = x, FUN = f, FUN.VALUE = logical(dim(x)[[-margin]]))
    count <- apply(X = count, MARGIN = margin, FUN = sum)
  } else {
    count <- apply(X = x, MARGIN = margin,
                   FUN = function(x, f) sum(f(x)), f = f)
  }
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
    k <- paste0(sprintf("%s (%d)", parts[index], count[index]), collapse = ", ")
    message(sprintf("Removed %d %s (%s): %s", n, mar, type, k))
  }

  index
}
