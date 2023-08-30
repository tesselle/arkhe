# DATA CLEANING: KEEP
#' @include AllGenerics.R
NULL

# keep =========================================================================
#' @export
#' @rdname keep
#' @aliases keep,ANY-method
setMethod(
  f = "keep",
  signature = c(x = "ANY"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE,
                        verbose = getOption("arkhe.verbose"), ...) {
    i <- detect(x, f = f, margin = margin, negate = negate, all = all, ...)
    discard_message(x, keep = i, margin = margin, verbose = verbose)
    if (any(margin == 1)) return(x[i, , drop = FALSE])
    if (any(margin == 2)) return(x[, i, drop = FALSE])
    i
  }
)

#' @export
#' @rdname keep
#' @aliases keep_rows,ANY-method
setMethod(
  f = "keep_rows",
  signature = c(x = "ANY"),
  definition = function(x, f, negate = FALSE, all = FALSE,
                        verbose = getOption("arkhe.verbose"), ...) {
    keep(x, f, margin = 1, negate = negate, all = all, verbose = verbose, ...)
  }
)

#' @export
#' @rdname keep
#' @aliases keep_cols,ANY-method
setMethod(
  f = "keep_cols",
  signature = c(x = "ANY"),
  definition = function(x, f, negate = FALSE, all = FALSE,
                        verbose = getOption("arkhe.verbose"), ...) {
    keep(x, f, margin = 2, negate = negate, all = all, verbose = verbose, ...)
  }
)
