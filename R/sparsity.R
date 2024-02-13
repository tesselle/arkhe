# DATA SUMMARY: SPARCITY
#' @include AllGenerics.R
NULL

#' @export
#' @rdname sparsity
#' @aliases sparsity,matrix-method
setMethod(
  f = "sparsity",
  signature = c(x = "matrix"),
  definition = function(x, count = FALSE) {
    zeros <- sum(count(x, f = is_zero_numeric, margin = 2, na.rm = TRUE))
    if (count) return(zeros)

    ## Proportion
    total <- prod(dim(x))
    zeros / total
  }
)

#' @export
#' @rdname sparsity
#' @aliases sparsity,data.frame-method
setMethod(
  f = "sparsity",
  signature = c(x = "data.frame"),
  definition = function(x, count = FALSE) {
    zeros <- sum(count(x, f = is_zero_numeric, margin = 2, na.rm = TRUE))
    if (count) return(zeros)

    ## Count numeric values only
    num <- vapply(X = x, FUN = is.numeric, FUN.VALUE = logical(1))
    total <- nrow(x) * sum(num)

    ## Proportion
    zeros / total
  }
)
