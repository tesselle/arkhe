# HELPERS

# Helpers ======================================================================
#' Helpers
#'
#' * `compact()` removes elements from a list or vector.
#' * `detect()` finds values in a list or vector according to a given predicate.
#' * `count()` counts values in a list or vector according to a given predicate.
#' * `extract()` extracts a string form another string based on a pattern.
#' * `\%o\%` allows for function composition.
#' * `\%||\%` allows to define a default value.
#' @param x,y An object.
#' @param f,g A [`function`]. In `compact()`, `detect()` and `count()` `f` must
#'  be a [`logical`] predicate.
#' @param pattern A [`character`] string containing a regular expression.
#' @references
#'  Wickham, H. (2014). *Advanced R*. London: Chapman & Hall. The R Series.
#' @family utilities
#' @keywords internal utilities
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
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

#' Build a Matrix
#'
#' Creates a matrix from the given set of values.
#' @return A [`matrix`].
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
make_names <- function(x, n = 0, prefix = "var") {
  if (is.null(x)) {
    x <- if (n > 0) paste0(prefix, seq_len(n)) else character(0)
  } else {
    x <- make.unique(as.character(x), sep = "_")
  }
  x
}

make_dimnames <- function(x) {
  list(
    make_names(dimnames(x)[[1L]], nrow(x), "row"),
    make_names(dimnames(x)[[2L]], ncol(x), "col")
  )
}

make_matrix <- function(data, nrow, ncol, byrow, dimnames, row, col) {

  if (nrow == 0 & ncol == 0) {
    mtx <- matrix(data = data, nrow = 0, ncol = 0)
    return(mtx)
  }

  n <- length(data)
  if (col) ncol <- n / nrow
  if (row) nrow <- n / ncol
  if (byrow) {
    dim(data) <- c(ncol, nrow)
    data <- t(data)
  } else {
    dim(data) <- c(nrow, ncol)
  }

  # Make dimnames
  row_names <- make_names(dimnames[[1L]], nrow, "row")
  column_names <- make_names(dimnames[[2L]], ncol, "col")
  dimnames(data) <- list(row_names, column_names)

  data
}

# UUID =========================================================================
#' UUID
#'
#' `generate_uuid()` generates a universally unique identifier (UUID Version
#' 4 and Variant 1).
#' @param x A [`character`] string (UUID).
#' @param seed A single [`integer`] specifying the seeds.
#'  If `NULL` (the default) the seed will be re-initialized.
#' @details
#'  As it relies on R's internal random number generators and so will suffer
#'  from the use of [set.seed()] in a session, the seed is re-initialized during
#'  execution (unless `seed` is not `NULL`). To prevent any side effects, the
#'  random number generator (RNG) state is saved and restored when the function
#'  exits.
#' @return A 36 characters long [`character`] string.
#' @seealso [set.seed()]
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
