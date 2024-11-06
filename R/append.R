# DATA CLEANING: APPEND
#' @include AllGenerics.R
NULL

#' @export
#' @rdname append_rownames
#' @aliases append_rownames,data.frame-method
setMethod(
  f = "append_rownames",
  signature = c(x = "data.frame"),
  definition = function(x, after = 0, remove = TRUE, var = "rownames") {
    assert_scalar(after, "numeric")
    assert_scalar(remove, "logical")
    assert_scalar(var, "character")

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

#' @export
#' @rdname append_column
#' @aliases append_column,data.frame-method
setMethod(
  f = "append_column",
  signature = c(x = "data.frame"),
  definition = function(x, column, after = 0, var = ".col") {
    assert_scalar(after, "numeric")
    assert_scalar(var, "character")
    if (!is_atomic(column)) {
      stop(sprintf("%s must be an atomic vector.", sQuote("x")), call. = FALSE)
    }

    m <- nrow(x)
    if (has_rownames(x) && has_names(column)) {
      i <- match(names(column), rownames(x))

      if (anyNA(i)) {
        column <- column[!is.na(i)]
        i <- i[!is.na(i)]
      }

      old_column <- column
      column <- rep(NA, m)
      column[i] <- old_column
    }

    assert_length(column, m)

    p <- ncol(x)
    if (after > p) after <- p
    i_before <- seq_len(after)
    i_after <- if (after < p) seq(from = after + 1, to = p, by = 1) else 0

    x <- cbind(x[, i_before, drop = FALSE], column, x[, i_after, drop = FALSE])

    colnames(x)[after + 1] <- var
    x
  }
)
