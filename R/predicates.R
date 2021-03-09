# PREDICATES

# Helpers ======================================================================
#' Utility Predicates
#'
#' * `is_empty()` checks if a vector or list is empty.
#' * `is_missing()` checks if a vector or list contains missing values.
#' * `is_named()` checks if an object is named.
#' * `is_uuid()` checks if a string is a canonically formatted UUID that is
#'   version 1 through 5 and is the appropriate Variant as per RFC4122.
#' @param x An object to be tested.
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-utils
#' @rdname predicate-utils
NULL

#' @export
#' @rdname predicate-utils
is_empty <- function(x) {
  length(x) == 0
}
#' @export
#' @rdname predicate-utils
is_named <- function(x) {
  !is_empty(names(x))
}

is_uuid <- function(x) {
  pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(pattern, x)
}

# Type =========================================================================
#' Type Predicates
#'
#' @param x An object to be tested.
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-type
#' @rdname predicate-type
NULL

#' @rdname predicate-type
is_list <- function(x) {
  typeof(x) == "list"
}
#' @rdname predicate-type
is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double",
                   "complex", "character", "raw")
}
#' @rdname predicate-type
is_vector <- function(x) {
  is_atomic(x) || is_list(x)
}
#' @rdname predicate-type
is_numeric <- function(x) {
  typeof(x) %in% c("integer", "double")
}
#' @rdname predicate-type
is_integer <- function(x) {
  typeof(x) == "integer"
}
#' @rdname predicate-type
is_double <- function(x) {
  typeof(x) == "double"
}
#' @rdname predicate-type
is_character <- function(x) {
  typeof(x) == "character"
}
#' @rdname predicate-type
is_logical <- function(x) {
  typeof(x) == "logical"
}
#' @rdname predicate-type
is_error <- function(x) {
  inherits(x, "try-error") || inherits(x, "simpleError") || inherits(x, "error")
}
#' @rdname predicate-type
is_warning <- function(x) {
  inherits(x, "simpleWarning") || inherits(x, "warning")
}
#' @rdname predicate-type
is_message <- function(x) {
  inherits(x, "simpleMessage") || inherits(x, "message")
}

# Scalar =======================================================================
#' Scalar Type Predicates
#'
#' @param x An object to be tested.
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-scalar
#' @rdname predicate-scalar
NULL

#' @rdname predicate-scalar
is_scalar_list <- function(x) {
  is_list(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_atomic <- function(x) {
  is_atomic(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_vector <- function(x) {
  is_vector(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_numeric <- function(x) {
  is_numeric(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_integer <- function(x) {
  is_integer(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_double <- function(x) {
  is_double(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_character <- function(x) {
  is_character(x) && length(x) == 1
}
#' @rdname predicate-scalar
is_scalar_logical <- function(x) {
  is_logical(x) && length(x) == 1
}

# Numeric ======================================================================
#' Numeric Predicates
#'
#' Check numeric objects:
#' * `is_zero()` checks if an object contains only zeros.
#' * `is_odd()` and `is_even()` check if a number is odd or even, respectively.
#' * `is_positive()` and `is_negative` check if an object contains only
#'   (strictly) positive or negative numbers.
#' * `is_whole()` checks if an object only contains whole numbers.
#' @param x A [`numeric`] object to be tested.
#' @param tolerance A [`numeric`] scalar giving the tolerance to check within.
#' @param strict A [`logical`] scalar: should strict inequality be used?
#' @param finite A [`logical`] scalar: should non-[`finite`] values also be
#'  removed?
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`)
#'  be omitted?
#' @return A [`logical`] vector.
#' @family predicates
#' @name predicate-numeric
#' @rdname predicate-numeric
NULL

#' @rdname predicate-numeric
is_missing <- function(x, finite = FALSE) {
  if (finite) !is.finite(x) else is.na(x)
}
#' @export
#' @rdname predicate-numeric
is_zero <- function(x, na.rm = FALSE) {
  check_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  x == 0
}
#' @export
#' @rdname predicate-numeric
is_odd <- function(x, na.rm = FALSE) { # impair
  check_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  as.logical(x %% 2)
}
#' @export
#' @rdname predicate-numeric
is_even <- function(x, na.rm = FALSE) { # pair
  check_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  !as.logical(x %% 2)
}
#' @export
#' @rdname predicate-numeric
is_positive <- function(x, strict = FALSE, na.rm = FALSE) {
  check_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  if (strict) x > 0 else x >= 0
}
#' @export
#' @rdname predicate-numeric
is_negative <- function(x, strict = FALSE, na.rm = FALSE) {
  check_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  if (strict) x < 0 else x <= 0
}
#' @export
#' @rdname predicate-numeric
is_whole <- function(x, na.rm = FALSE, tolerance = .Machine$double.eps^0.5) {
  check_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  abs(x - round(x, digits = 0)) <= tolerance
}

#' Numeric Trend Predicates
#'
#' Check numeric objects:
#' * `is_constant()` checks for equality among all elements of a vector.
#' * `is_increasing()` and `is_decreasing()` check if a sequence of numbers
#'   is monotonically increasing or decreasing, respectively.
#' * `is_overlapping()` checks if two data ranges overlap at all.
#' @param x A [`numeric`] object to be tested.
#' @param tolerance A [`numeric`] scalar giving the tolerance to check within.
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`)
#'  be omitted?
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-trend
#' @rdname predicate-trend
NULL

#' @export
#' @rdname predicate-trend
is_constant <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  check_type(x, "numeric")

  k <- abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
  if (is.na(k)) k <- FALSE
  k
}
#' @export
#' @rdname predicate-trend
is_increasing <- function(x, na.rm = TRUE) {
  check_type(x, "numeric")

  k <- all(x == cummax(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
#' @export
#' @rdname predicate-trend
is_decreasing <- function(x, na.rm = TRUE) {
  check_type(x, "numeric")

  k <- all(x == cummin(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}

# Matrix =======================================================================
#' Matrix Predicates
#'
#' * `is_square()` checks if a matrix is square.
#' * `is_symmetric()` checks if a matrix is symmetric.
#' @param x A [`matrix`] to be tested.
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-matrix
#' @rdname predicate-matrix
NULL

#' @export
#' @rdname predicate-matrix
is_square <- function(x) {
  if (is.matrix(x)) nrow(x) == ncol(x) else FALSE
}
#' @export
#' @rdname predicate-matrix
is_symmetric <- function(x) {
  if (is.matrix(x)) identical(x, t(x)) else FALSE
}

# Graph ========================================================================
#' Graph Predicates
#'
#' `is_dag()` checks if a graph has a topological ordering (i.e. is a directed
#' acyclic graph) using Kahn's algorithm.
#' @param x An adjacency [`matrix`] to be tested.
#' @return A [`logical`] scalar.
#' @references
#'  Kahn, A. B. (1962). Topological sorting of large networks. *Communications
#'  of the ACM*, 5(11), p. 558-562. \doi{10.1145/368996.369025}.
#' @family predicates
#' @name predicate-graph
#' @rdname predicate-graph
#' @keywords internal
NULL

#' @export
#' @rdname predicate-graph
is_dag <- function(x) {
  # Get edges
  G <- matrix2edges(x)
  # Find nodes which have no incoming edges
  S <- which(colSums(x) == 0)
  # List that will contain the sorted elements
  L <- list()

  if (length(S) == 0)
    return(FALSE)

  k <- 1L
  while (k == 1 || length(S) != 0) {
    # Remove a node n from S and add n to tail of L
    n <- S[[1]]
    S <- S[-1]
    L <- append(L, n)
    # For each node m with an edge e from n to m
    e <- which(G[, 1] == n)
    m <- G[e, 2]
    # Do remove edge e from the graph
    G <- G[-e, , drop = FALSE]
    # If m has no other incoming edges then insert m into S
    if (nrow(G) != 0) {
      m <- m[!(m %in% G[, 2])]
      if (length(m) != 0)
        S <- append(S, m)
    }
    k <- k + 1
  }
  return(nrow(G) == 0)
}
