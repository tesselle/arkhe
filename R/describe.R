# DATA SUMMARY: DESCRIBE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname describe
#' @aliases describe,ANY-method
setMethod(
  f = "describe",
  signature = c(x = "ANY"),
  definition = function(x) {
    ## Dimensions
    m <- nrow(x)
    p <- ncol(x)

    rows <- ngettext(m, "observation", "observations")
    cols <- ngettext(p, "variable", "variables")

    msg_tbl <- sprintf("%d %s with %d %s:", m, rows, p, cols)

    ## Missing values
    m_NA <- sum(detect(x, f = is.na, margin = 1))
    p_NA <- sum(detect(x, f = is.na, margin = 2))

    rows_NA <- ngettext(m_NA, "observation", "observations")
    cols_NA <- ngettext(p_NA, "variable", "variables")

    pc <- label_percent(c(m_NA / m, p_NA / p), digits = 1, trim = TRUE)
    pc_NA <- sprintf(" (%s)", pc)

    msg_row_NA <- sprintf("%d %s%s containing missing values.", m_NA, rows_NA, pc_NA[[1]])
    msg_col_NA <- sprintf("%d %s%s containing missing values.", p_NA, cols_NA, pc_NA[[2]])

    ## Constant columns
    p_var <- sum(detect(x, f = function(x) is_unique(x), margin = 2))
    cols_var <- ngettext(p_var, "variable", "variables")
    msg_col_var <- sprintf("%d %s with no variance.", p_var, cols_var)

    ## Sparsity
    spa <- sparsity(x, count = FALSE)
    msg_spa <- sprintf("%s of numeric values are zero.", label_percent(spa, digits = 1))

    ## Variable types
    num <- detect(x, f = is.numeric, margin = 2)
    bin <- detect(x, f = is.logical, margin = 2)
    n_num <- sum(num)
    n_bin <- sum(bin)
    n_cha <- sum(!num & !bin)

    msg_num <- sprintf("%d numeric %s.", n_num, ngettext(n_num, "variable", "variables"))
    msg_bin <- sprintf("%d binary %s.", n_bin, ngettext(n_bin, "variable", "variables"))
    msg_cha <- sprintf("%d categorial %s.", n_cha, ngettext(n_cha, "variable", "variables"))

    cat(msg_tbl, msg_num, msg_cha, msg_bin, sep = "\n* ")
    cat("\nData checking:", msg_spa, msg_col_var, sep = "\n* ")
    cat("\nMissing values:", msg_row_NA, msg_col_NA, sep = "\n* ")

    # tot <- list(
    #   m = m, p = p,
    #   n_numeric = n_num, n_categorial = n_cha, n_binary = n_bin,
    #   row_missing = m_NA, col_missing = p_NA,
    #   zero_values = spa, zero_variance = p_var
    # )
    invisible(x)
  }
)

#' Label Percentages
#'
#' @param x A [`numeric`] vector.
#' @param digits An [`integer`] indicating the number of decimal places.
#'  If `NULL` (the default), breaks will have the minimum number of digits
#'  needed to show the difference between adjacent values.
#' @param trim A [`logical`] scalar. If `FALSE` (the default), values are
#'  right-justified to a common width (see [base::format()]).
#' @return A [`character`] vector.
#' @keywords internal
#' @export
label_percent <- function(x, digits = NULL, trim = FALSE) {
  i <- !is.na(x)
  y <- x[i]
  y <- abs(y) * 100
  y <- format(y, trim = trim, digits = digits)
  y <- paste0(y, "%")
  x[i] <- y
  x
}
