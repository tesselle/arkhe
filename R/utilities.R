# HELPERS

#' Helpers
#'
#' \code{compact} removes elements from a list or vector.
#' \code{detect} finds values in a list or vector according to a given
#' predicate.
#' \code{count} counts values in a list or vector according to a given
#' predicate.
#' \code{extract} extracts a string form another string based on a pattern.
#'
#' \code{\%o\%} allows for function composition.
#' \code{\%||\%} allows to define a default value.
#' @param x,y An object.
#' @param f,g A \code{\link{function}}. In \code{compact}, \code{detect}
#'  and \code{count} \code{f} must be a \code{\link{logical}} predicate.
#' @param pattern A \code{\link{character}} string containing a regular
#'  expression.
#' @references
#'  Wickham, H. (2014). \emph{Advanced R}. London: Chapman & Hall. The R Series.
#' @family utilities
#' @keywords internal utilities
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) || length(x) != 0) x else y
}
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
compact <- function(f, x) {
  Filter(Negate(f), x)
}
detect <- function(f, x) {
  vapply(x, f, logical(1))
}
count <- function(f, x) {
  sum(detect(f, x))
}
extract <- function(x, pattern) {
  regmatches(x, regexpr(pattern, x))
}

#' UUID
#'
#' \code{generate_uuid} generates a universally unique identifier (UUID Version
#' 4 and Variant 1).
#' @param x A \code{\link{character}} string (UUID).
#' @param seed A single \code{\link{integer}} specifying the seeds.
#'  If \code{NULL} (the default) the seed will be re-initialized.
#' @details
#'  As it relies on R's internal random number generators and so will suffer
#'  from the use of \code{\link{set.seed}} in a session, the seed is
#'  re-initialized during execution (unless \code{seed} is not \code{NULL}).
#'  To prevent any side effects, the random number generator (RNG) state is
#'  saved and restored when the function exits.
#' @return A 36 characters long \code{\link{character}} string.
#' @seealso \link{set.seed}
#' @author N. Frerebeau
#' @keywords internal
generate_uuid <- function(seed = NULL) {
  # Save and restore the random number generator (RNG) state
  if (!exists(".Random.seed", mode = "numeric")) sample(NA)
  old_seed <- .Random.seed
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  # Set seed
  seed <- if (is.numeric(seed)) seed else NULL
  set.seed(seed = seed)

  # Generate 32 pseudo random hex digits
  hex_digits <- c(as.character(0:9), letters[seq_len(6)])
  hex_32 <- sample(hex_digits, size = 32, replace = TRUE)
  # Set version (4) and variant (1)
  hex_32[13] <- 4
  hex_32[17] <- sample(c(8, 9, "a", "b"), size = 1)

  uuid <- paste(
    mapply(
      FUN = substr,
      start = c(1, 9, 13, 17, 21),
      stop = c(8, 12, 16, 20, 32),
      MoreArgs = list(x = paste0(hex_32, collapse = "")),
      SIMPLIFY = FALSE
    ),
    collapse = "-"
  )
  uuid
}

#' Row and Column Names
#'
#' \code{rownames_to_column} converts row names to an explicit column.
#' @param x A \code{\link{matrix}} or \code{\link{data.frame}}.
#' @param factor A \code{\link{logical}} scalar: should row names be coerced to
#'  factors? The default (\code{TRUE}) preserves the original ordering of the
#'  columns.
#' @param id A \code{\link{character}} string giving the name of the newly
#'  created column.
#' @return A data.frame
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
rownames_to_column <- function(x, factor = TRUE, id = "id") {
  if (!is.matrix(x) && !is.data.frame(x))
    stop("A matrix or data.frame is expected.", call. = FALSE)

  if (is.null(colnames(x))) {
    colnames(x) <- paste0("col", seq_len(ncol(x)))
  }
  row_names <- rownames(x)
  if (is.null(row_names)) {
    row_names <- paste0("row", seq_len(nrow(x)))
  }
  if (factor) {
    row_names <- factor(x = row_names, levels = row_names)
  }

  z <- cbind.data.frame(row_names, x, stringsAsFactors = FALSE)
  colnames(z) <- c(id, colnames(x))
  rownames(z) <- NULL
  z
}

#' Row and Column Indexes
#'
#' Returns a vector of integer their row or column number in a matrix-like
#' object.
#' @param x A length-two \code{\link{integer}} vector giving the dimension of a
#'  matrix-like object.
#' @return An \code{\link{integer}} vector.
#' @examples
#'  mtx <- matrix(data = c(1,2,3,4,5,6), nrow = 2)
#'  all(index_by_row(c(2, 3)) == row(mtx))
#'  all(index_by_column(c(2, 3)) == col(mtx))
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
index_by_row <- function(x) {
  if (!is.numeric(x) || length(x) != 2)
    stop(sprintf("% must be a length-two numeric vector.", sQuote("x")),
         call. = FALSE)
  rep(seq_len(x[[1L]]), times = x[[2L]])
}
index_by_column <- function(x) {
  if (!is.numeric(x) || length(x) != 2)
    stop(sprintf("% must be a length-two numeric vector.", sQuote("x")),
         call. = FALSE)
  rep(seq_len(x[[2L]]), each = x[[1L]])
}

#' Make Unique Row or Column Names
#'
#' Returns a vector of names.
#' @param x A \code{\link{character}} vector giving the dimension names of a
#'  matrix-like object.
#' @param n An \code{\link{integer}} scalar giving the dimension.
#' @param prefix A \code{\link{character}} string specifying the prefix to be
#'  used to make dimnames.
#' @return An \code{\link{character}} vector.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
make_names <- function(x, n = NULL, prefix = "row") {
  if (is.null(x)) {
    if (!is.null(n) && n > 0) paste0(prefix, seq_len(n)) else character(0)
  } else {
    make.unique(as.character(x), sep = "_")
  }
}
make_dimnames <- function(x) {
  size <- dim(x)
  dim_names <- dimnames(x)
  list(
    make_names(dim_names[[1L]], size[[1L]], "row"),
    make_names(dim_names[[2L]], size[[2L]], "col")
  )
}
