# DATA CLEANING: COMPACT
#' @include AllGenerics.R
NULL

## compact rows/columns --------------------------------------------------------
#' @export
#' @rdname compact
#' @aliases compact,ANY-method
setMethod(
  f = "compact",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1) {
    vide <- function(x) {
      miss <- is.na(x)
      if (is_numeric(x)) x == 0 | miss
      else if (is_character(x)) x == "" | miss
      else miss
    }
    discard(x, f = vide, margin = margin, all = TRUE)
  }
)

#' @export
#' @rdname compact
#' @aliases compact_cols,ANY-method
setMethod(
  f = "compact_cols",
  signature = signature(x = "ANY"),
  definition = function(x) {
    compact(x, margin = 2)
  }
)

#' @export
#' @rdname compact
#' @aliases compact_rows,ANY-method
setMethod(
  f = "compact_rows",
  signature = signature(x = "ANY"),
  definition = function(x) {
    compact(x, margin = 1)
  }
)
