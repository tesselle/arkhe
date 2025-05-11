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
    ## Variable types
    msg_var <- describe_variables(x)

    ## Missing values
    msg_miss <- describe_missing(x)

    ## Check
    msg_val <- describe_check(x)

    cat(msg_var, msg_miss, msg_val, sep = "\n")

    # tot <- list(
    #   m = m, p = p,
    #   n_numeric = n_num, n_categorial = n_cha, n_binary = n_bin,
    #   row_missing = m_NA, col_missing = p_NA,
    #   zero_values = spa, zero_variance = p_var
    # )

    invisible(x)
  }
)

describe_variables <- function(x) {
  m <- nrow(x)
  p <- ncol(x)

  msg_rows <- sprintf(ngettext(m, "%d observation", "%d observations"), m)
  msg_cols <- sprintf(ngettext(p, "%d variable", "%d variables"), p)
  title <- sprintf("%s, %s:", msg_rows, msg_cols)

  num <- detect(x, f = is.numeric, margin = 2)
  bin <- detect(x, f = is.logical, margin = 2)
  n_num <- sum(num)
  n_bin <- sum(bin)
  n_cha <- sum(!num & !bin)

  msg_num <- sprintf(ngettext(n_num, "%d numeric variable", "%d numeric variables"), n_num)
  msg_bin <- sprintf(ngettext(n_bin, "%d binary variable", "%d binary variables"), n_bin)
  msg_cha <- sprintf(ngettext(n_cha, "%d categorial variable", "%d categorial variables"), n_cha)

  msg <- paste0(sprintf("\n* %s.", c(msg_num, msg_cha, msg_bin)), collapse = "")
  paste0(title, msg, collapse = "")
}
describe_missing <- function(x) {
  m <- nrow(x)
  p <- ncol(x)

  n_NA <- sum(count(x, f = is.na))
  m_NA <- sum(detect(x, f = is.na, margin = 1))
  p_NA <- sum(detect(x, f = is.na, margin = 2))
  pc <- label_percent(c(m_NA / m, p_NA / p), digits = 1, trim = TRUE)

  title <- sprintf(ngettext(n_NA, "%d missing value:", "%d missing values:"), n_NA)

  rows_NA <- ngettext(m_NA, "%d observation (%s) contains missing values",
                      "%d observations (%s) contain missing values")
  msg_row_NA <- sprintf(rows_NA, m_NA, pc[[1]])

  cols_NA <- ngettext(p_NA, "%d variable (%s) contains missing values",
                      "%d variables (%s) contain missing values")
  msg_col_NA <- sprintf(cols_NA, p_NA, pc[[2]])

  msg <- paste0(sprintf("\n* %s.", c(msg_row_NA, msg_col_NA)), collapse = "")
  paste0("\n", title, msg, collapse = "")
}
describe_check <- function(x) {
  title <- tr_("Data checking:")

  ## Constant columns
  p_var <- sum(detect(x, f = function(x) is_unique(x), margin = 2))
  cols_var <- ngettext(p_var, "%d variable with no variance",
                       "%d variables with no variance")
  msg_col_var <- sprintf(cols_var, p_var)

  ## Sparsity
  spa <- sparsity(x, count = FALSE)
  msg_spa <- sprintf(tr_("%s of numeric values are zero"), label_percent(spa, digits = 1))

  msg <- paste0(sprintf("\n* %s.", c(msg_spa, msg_col_var)), collapse = "")
  paste0("\n", title, msg, collapse = "")
}

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
