# MATHEMATICS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname math_gcd
#' @aliases math_gcd,numeric,numeric-method
setMethod(
  f = "math_gcd",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    mapply(
      FUN = function(a, b) {
        if (isTRUE(all.equal(b, 0))) return(a)
        math_gcd(b, a %% b)
      },
      a = x,
      b = y
    )
  }
)

#' @export
#' @rdname math_lcm
#' @aliases math_lcm,numeric,numeric-method
setMethod(
  f = "math_lcm",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    (x * y) / math_gcd(x, y)
  }
)
