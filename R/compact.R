# DATA CLEANING: COMPACT
#' @include AllGenerics.R
NULL

## compact rows/columns ========================================================
#' @export
#' @rdname compact
#' @aliases compact,ANY-method
setMethod(
  f = "compact",
  signature = c(x = "ANY"),
  definition = function(x, margin = 1, na.rm = FALSE,
                        verbose = getOption("arkhe.verbose")) {
    vide <- function(x) {
      if (is_numeric(x)) x == 0
      else if (is_logical(x)) !x
      else if (is_character(x)) x == ""
      else rep(FALSE, length(x))
    }
    discard(x, f = vide, margin = margin, all = TRUE,
            na.rm = na.rm, verbose = verbose)
  }
)

#' @export
#' @rdname compact
#' @aliases compact_columns,ANY-method
setMethod(
  f = "compact_columns",
  signature = c(x = "ANY"),
  definition = function(x, na.rm = FALSE, verbose = getOption("arkhe.verbose")) {
    compact(x, margin = 2, na.rm = na.rm, verbose = verbose)
  }
)

#' @export
#' @rdname compact
#' @aliases compact_rows,ANY-method
setMethod(
  f = "compact_rows",
  signature = c(x = "ANY"),
  definition = function(x, na.rm = FALSE, verbose = getOption("arkhe.verbose")) {
    compact(x, margin = 1, na.rm = na.rm, verbose = verbose)
  }
)
