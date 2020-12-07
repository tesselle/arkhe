# COMPOSITIONAL DATA
#' @include AllGenerics.R AllClasses.R
NULL

#' Geometric Mean
#'
#' @param x A \code{\link{numeric}} vector.
#' @param na.rm A \code{\link{logical}} scalar: should missing values be
#'  stripped before the computation proceeds?
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
gmean <- function(x, na.rm = FALSE) {
  index <- if (na.rm) is.finite(x) & x > 0 else x > 0
  exp(mean(log(unclass(x)[index])))
}

#' Centered Log-Ratio
#'
#' @param x A \code{\link{numeric}} vector.
#' @param base A \code{\link{numeric}} value giving the base with respect to
#'  which logarithms are computed.
#' @param na.rm A \code{\link{logical}} scalar: should missing values be
#'  stripped before the computation proceeds?
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
clr <- function(x, base = exp(1), na.rm = FALSE) {
  log(x / gmean(x, na.rm = na.rm), base = base)
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
#' @aliases var,AbundanceMatrix-method
setMethod(
  f = "var",
  signature = signature(x = "AbundanceMatrix", y = "missing"),
  definition = function(x) {
    (2 * ncol(x))^(-1) * sum(variation(x))
  }
)

#' @export
#' @rdname compositions
#' @aliases sd,AbundanceMatrix-method
setMethod(
  f = "sd",
  signature = signature(x = "AbundanceMatrix"),
  definition = function(x) {
    sqrt((ncol(x) - 1)^(-1) * var(x))
  }
)

#' @export
#' @rdname compositions
#' @aliases variation,AbundanceMatrix-method
setMethod(
  f = "variation",
  signature = signature(x = "AbundanceMatrix"),
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

#' @export
#' @rdname compositions
#' @aliases dist,AbundanceMatrix-method
setMethod(
  f = "dist",
  signature = signature(x = "AbundanceMatrix"),
  definition = function(x, diag = FALSE, upper = FALSE) {
    n <- nrow(x)
    ij <- utils::combn(n, m = 2)
    pair <- seq_len(ncol(ij))
    d <- matrix(data = 0, nrow = n, ncol = n,
                dimnames = list(rownames(x), rownames(x)))

    for (k in pair) {
      i <- ij[1, k]
      j <- ij[2, k]
      z <- sqrt(sum(clr(x[i, ]) - clr(x[j, ]))^2)
      d[i, j] <- d[j, i] <- z
    }

    stats::as.dist(d, diag = diag, upper = upper)
  }
)
