# DATA CLEANING: DISCARD
#' @include AllGenerics.R
NULL

# discard ======================================================================
#' @export
#' @rdname discard
#' @aliases discard,ANY-method
setMethod(
  f = "discard",
  signature = c(x = "ANY"),
  definition = function(x, f, margin = 1, negate = FALSE, all = FALSE,
                        verbose = getOption("arkhe.verbose"), ...) {
    i <- detect(x, f = f, margin = margin, negate = negate, all = all, ...)
    discard_message(x, keep = !i, margin = margin, verbose = verbose)
    if (any(margin == 1)) return(x[!i, , drop = FALSE])
    if (any(margin == 2)) return(x[, !i, drop = FALSE])
    i
  }
)

#' @export
#' @rdname discard
#' @aliases discard_rows,ANY-method
setMethod(
  f = "discard_rows",
  signature = c(x = "ANY"),
  definition = function(x, f, negate = FALSE, all = FALSE,
                        verbose = getOption("arkhe.verbose"), ...) {
    discard(x, f, margin = 1, negate = negate, all = all, verbose = verbose, ...)
  }
)

#' @export
#' @rdname discard
#' @aliases discard_cols,ANY-method
setMethod(
  f = "discard_cols",
  signature = c(x = "ANY"),
  definition = function(x, f, negate = FALSE, all = FALSE,
                        verbose = getOption("arkhe.verbose"), ...) {
    discard(x, f, margin = 2, negate = negate, all = all, verbose = verbose, ...)
  }
)

#' Diagnostic Message
#'
#' Generates a diagnostic message describing columns or rows that are being
#' removed.
#' @param x A [`matrix`] or a [`data.frame`].
#' @param margin A length-one [`numeric`] vector giving the subscripts which the
#'  function will be applied over (`1` indicates rows, `2` indicates columns).
#' @param keep A [`logical`] vector of rows or columns to keep (`TRUE`) or
#'  remove (`FALSE`).
#' @param verbose A [`logical`] scalar: should the message be generated?
#' @keywords internal
#' @noRd
discard_message <- function(x, keep, margin,
                            verbose = getOption("arkhe.verbose")) {
  drop <- sum(!keep)
  what <- ngettext(drop, "element", "elements")
  if (any(margin == 1)) what <- ngettext(drop, "row", "rows")
  if (any(margin == 2)) what <- ngettext(drop, "column", "columns")

  if (drop == 0) {
    msg <- sprintf("No %s to remove.", what)
  } else {
    pc <- sprintf("%0.3g%%", 100 * drop / length(keep))
    if (margin == 2 && !is.null(colnames(x))) {
      pc <- paste0(colnames(x)[!keep], collapse = ", ")
    }
    msg <- "Removing %g %s out of %g (%s)."
    msg <- sprintf(msg, drop, what, length(keep), pc)
  }

  if (verbose) message(msg)
  invisible(msg)
}
