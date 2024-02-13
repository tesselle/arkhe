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

    msg_row_NA <- sprintf("%d %s%s containing missing values", m_NA, rows_NA, pc_NA[[1]])
    msg_col_NA <- sprintf("%d %s%s containing missing values", p_NA, cols_NA, pc_NA[[2]])

    ## Constant columns
    p_var <- sum(detect(x, f = function(x) is_unique(x), margin = 2))
    cols_var <- ngettext(p_var, "variable", "variables")
    msg_col_var <- sprintf("%d %s with no variance", p_var, cols_var)

    cat(msg_tbl, msg_row_NA, msg_col_NA, msg_col_var, sep = "\n* ")

    # tot <- list(
    #   m = m, p = p,
    #   row_missing = m_NA, col_missing = p_NA,
    #   col_constant = p_var
    # )
    invisible(x)
  }
)
