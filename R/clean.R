# DATA CLEANING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname clean
#' @aliases remove_missing,matrix-method
setMethod(
  f = "remove_missing",
  signature = signature(object = "matrix"),
  definition = function(object, margin = 1) {
    index <- detect_missing(object, margin = margin)
    if (margin == 1) x <- object[!index, ]
    if (margin == 2) x <- object[, !index]
    x
  }
)

#' @export
#' @rdname clean
#' @aliases remove_missing,data.frame-method
setMethod(
  f = "remove_missing",
  signature = signature(object = "data.frame"),
  definition = function(object, margin = 1) {
    index <- detect_missing(object, margin = margin)
    if (margin == 1) x <- object[!index, ]
    if (margin == 2) x <- object[, !index]
    x
  }
)

#' @export
#' @rdname clean
#' @aliases remove_zero,matrix-method
setMethod(
  f = "remove_zero",
  signature = signature(object = "matrix"),
  definition = function(object, margin = 1) {
    index <- detect_zero(object, margin = margin)
    if (margin == 1) x <- object[!index, ]
    if (margin == 2) x <- object[, !index]
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
    if (margin == 1) x <- object[!index, ]
    if (margin == 2) x <- object[, !index]
    x
  }
)

#' Data Cleaning
#'
#' @param x An object. It will be coerced to a \code{matrix} as by
#'  \code{\link{as.matrix}}.
#' @param margin A vector giving the subscripts which the function will be
#'  applied over (see \code{\link{apply}}).
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
detect_missing <- function(x, margin = 1) {
  detect_any(x, f = is.na, margin = margin, type = "NAs")
}
detect_zero <- function(x, margin = 1) {
  is.zero <- function(x) {
    num <- suppressWarnings(try(as.numeric(x), silent = TRUE))
    if (!inherits(num, "try-error"))
      stats::na.omit(num) == 0
    else FALSE
  }
  detect_any(x, f = is.zero, margin = margin, type = "zeros")
}
detect_any <- function(x, f, margin = 1, type = "generic") {
  parts <- dimnames(x)[[margin]]

  count <- apply(X = x, MARGIN = margin, FUN = function(x, f) sum(f(x)), f = f)
  index <- count > 0

  n <- sum(index)
  if (n > 0 & getOption("nexus.verbose")) {
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
