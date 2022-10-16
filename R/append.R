# DATA CLEANING: APPEND
#' @include AllGenerics.R
NULL

#' @export
#' @rdname append
#' @aliases append_rownames,data.frame-method
setMethod(
  f = "append_rownames",
  signature = signature(x = "data.frame"),
  definition = function(x, after = 0, remove = TRUE, var = "rownames") {
    ## Validation
    assert_length(after, 1)

    n <- ncol(x)
    if (after > n) after <- n
    i_before <- seq_len(after)
    i_after <- if (after < n) seq(from = after + 1, to = n, by = 1) else 0

    z <- rownames(x)
    x <- cbind(x[, i_before, drop = FALSE], z, x[, i_after, drop = FALSE])

    colnames(x)[after + 1] <- var
    rownames(x) <- if (remove) NULL else z

    x
  }
)
