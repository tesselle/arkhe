# DATA CLEANING: REMOVE
#' @include AllGenerics.R
NULL

# Missing values ===============================================================
#' @export
#' @rdname remove_NA
#' @aliases remove_NA,ANY-method
setMethod(
  f = "remove_NA",
  signature = c(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE,
                        verbose = getOption("arkhe.verbose")) {
    discard(x, f = is.na, margin = margin, all = all, verbose = verbose)
  }
)

# Infinite values ==============================================================
#' @export
#' @rdname remove_Inf
#' @aliases remove_Inf,ANY-method
setMethod(
  f = "remove_Inf",
  signature = c(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE,
                        verbose = getOption("arkhe.verbose")) {
    discard(x, f = is.infinite, margin = margin, all = all, verbose = verbose)
  }
)

# Zeros ========================================================================
#' @export
#' @rdname remove_zero
#' @aliases remove_zero,ANY-method
setMethod(
  f = "remove_zero",
  signature = c(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE,
                        verbose = getOption("arkhe.verbose")) {
    discard(x, f = is_zero_numeric, margin = margin,
            all = all, na.rm = TRUE, verbose = verbose)
  }
)

# Empty string =================================================================
#' @export
#' @rdname remove_empty
#' @aliases remove_empty,ANY-method
setMethod(
  f = "remove_empty",
  signature = c(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE,
                        verbose = getOption("arkhe.verbose")) {
    discard(x, f = is_empty_string, margin = margin,
            all = all, na.rm = TRUE, verbose = verbose)
  }
)

# Constant =====================================================================
#' @export
#' @rdname remove_constant
#' @aliases remove_constant,ANY-method
setMethod(
  f = "remove_constant",
  signature = c(x = "ANY"),
  definition = function(x, na.rm = FALSE, verbose = getOption("arkhe.verbose")) {
    discard(x, f = function(x) { is_unique(x, na.rm) },
            margin = 2, all = FALSE, verbose = verbose)
  }
)
