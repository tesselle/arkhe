# COMPOSITIONAL DATA
#' @include AllGenerics.R AllClasses.R
NULL

#' Geometric Mean
#'
#' @param x A \code{\link{numeric}} vector.
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
gmean <- function(x, na.rm = FALSE) {
  index <- if (na.rm) is.finite(x) & x > 0 else x > 0
  exp(mean(log(unclass(x)[index])))
}

#' @export
#' @rdname compositions
#' @aliases mean,AbundanceMatrix-method
setMethod(
  f = "mean",
  signature = signature(x = "AbundanceMatrix"),
  definition = function(x, na.rm = FALSE) {
    x <- apply(X = x, MARGIN = 2, FUN = gmean, na.rm = na.rm)
    x / sum(x)
  }
)

#' @export
#' @rdname compositions
#' @aliases cov,AbundanceMatrix-method
setMethod(
  f = "cov",
  signature = signature(x = "AbundanceMatrix", y = "missing"),
  definition = function(x) {
    geo_mean <- apply(X = x, MARGIN = 1, FUN = gmean)
    clr <- log(x / geo_mean, base = exp(1))
    cov(clr)
  }
)


#' @export
#' @rdname compositions
#' @aliases cor,AbundanceMatrix-method
setMethod(
  f = "cor",
  signature = signature(x = "AbundanceMatrix", y = "missing"),
  definition = function(x) {
    tau <- var(x)
    rho <- exp( -tau^2 / 2)
    rho
  }
)

#' @export
#' @rdname compositions
#' @aliases var,AbundanceMatrix-method
setMethod(
  f = "var",
  signature = signature(x = "AbundanceMatrix", y = "missing"),
  definition = function(x) {

    n <- ncol(x)
    ij <- utils::combn(n, m = 2)
    pair <- seq_len(ncol(ij))
    tau <- matrix(data = 0, nrow = n, ncol = n,
                  dimnames = list(colnames(x), colnames(x)))

    for (k in pair) {
      i <- ij[1, k]
      j <- ij[2, k]
      z <- var(log(x[, i] / x[, j], base = exp(1)))
      tau[i, j] <- tau[j, i] <- z
    }
    tau
  }
)
