# DATA CLEANING: REMOVE
#' @include AllGenerics.R
NULL

# Missing values ===============================================================
#' @export
#' @rdname missing
#' @aliases remove_NA,ANY-method
setMethod(
  f = "remove_NA",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    discard(x, f = is.na, margin = margin, all = all)
  }
)

# Infinite values ==============================================================
#' @export
#' @rdname infinite
#' @aliases remove_Inf,ANY-method
setMethod(
  f = "remove_Inf",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE) {
    discard(x, f = is.infinite, margin = margin, all = all)
  }
)

# Zeros ========================================================================
#' @export
#' @rdname zero
#' @aliases remove_zero,ANY-method
setMethod(
  f = "remove_zero",
  signature = signature(x = "ANY"),
  definition = function(x, margin = 1, all = FALSE, na.rm = FALSE) {
    discard(x, f = is_zero, margin = margin, all = all, na.rm = na.rm)
  }
)

# Constant =====================================================================
#' @export
#' @rdname remove_constant
#' @aliases remove_constant,data.frame-method
setMethod(
  f = "remove_constant",
  signature = signature(x = "data.frame"),
  definition = function(x, na.rm = FALSE) {
    all_unique <- function(x, na.rm = FALSE) {
      if (na.rm) x <- stats::na.omit(x)
      length(unique(x)) <= 1
    }
    i <- vapply(X = x, FUN = all_unique, FUN.VALUE = logical(1), na.rm = na.rm)
    x[, !i, drop = FALSE]
  }
)

#' @export
#' @rdname remove_constant
#' @aliases remove_constant,matrix-method
setMethod(
  f = "remove_constant",
  signature = signature(x = "matrix"),
  definition = function(x, na.rm = FALSE) {
    x <- as.data.frame(x)
    methods::callGeneric(x, na.rm = na.rm)
  }
)
