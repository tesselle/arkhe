# DATA CLEANING: REPLACE
#' @include AllGenerics.R
NULL

# Replace ======================================================================
## Missing values --------------------------------------------------------------
#' @export
#' @rdname replace_NA
#' @aliases replace_NA,matrix-method
setMethod(
  f = "replace_NA",
  signature = c(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.na(x)] <- value
    x
  }
)

#' @export
#' @rdname replace_NA
#' @aliases replace_NA,data.frame-method
setMethod(
  f = "replace_NA",
  signature = c(x = "data.frame"),
  definition = function(x, value = 0) {
    x[] <- lapply(
      X = x,
      FUN = function(x, value) {
        x[is.na(x)] <- value
        x
      },
      value = value
    )
    x
  }
)

## Infinite values -------------------------------------------------------------
#' @export
#' @rdname replace_Inf
#' @aliases replace_Inf,matrix-method
setMethod(
  f = "replace_Inf",
  signature = c(x = "matrix"),
  definition = function(x, value = 0) {
    x[is.infinite(x)] <- value
    x
  }
)

#' @export
#' @rdname replace_Inf
#' @aliases replace_Inf,data.frame-method
setMethod(
  f = "replace_Inf",
  signature = c(x = "data.frame"),
  definition = function(x, value = 0) {
    x[] <- lapply(
      X = x,
      FUN = function(x, value) {
        x[is.infinite(x)] <- value
        x
      },
      value = value
    )
    x
  }
)

## Zeros -----------------------------------------------------------------------
#' @export
#' @rdname replace_zero
#' @aliases replace_zero,matrix-method
setMethod(
  f = "replace_zero",
  signature = c(x = "matrix"),
  definition = function(x, value) {
    x[is_zero(x)] <- value
    x
  }
)

#' @export
#' @rdname replace_zero
#' @aliases replace_zero,data.frame-method
setMethod(
  f = "replace_zero",
  signature = c(x = "data.frame"),
  definition = function(x, value) {
    num <- vapply(X = x, FUN = is.numeric, FUN.VALUE = logical(1))
    nozero <- lapply(
      X = x[, num, drop = FALSE],
      FUN = function(x, value) {
        x[is_zero(x)] <- value
        x
      },
      value = value
    )
    x[, num] <- nozero
    x
  }
)

## Empty string ----------------------------------------------------------------
#' @export
#' @rdname replace_empty
#' @aliases replace_empty,matrix-method
setMethod(
  f = "replace_empty",
  signature = c(x = "matrix"),
  definition = function(x, value) {
    x[!nzchar(x)] <- value
    x
  }
)

#' @export
#' @rdname replace_empty
#' @aliases replace_empty,data.frame-method
setMethod(
  f = "replace_empty",
  signature = c(x = "data.frame"),
  definition = function(x, value) {
    char <- vapply(X = x, FUN = is.character, FUN.VALUE = logical(1))
    noblank <- lapply(
      X = x[, char, drop = FALSE],
      FUN = function(x, value) {
        x[!nzchar(x)] <- value
        x
      },
      value = value
    )
    x[, char] <- noblank
    x
  }
)
