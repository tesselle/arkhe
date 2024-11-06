# PREDICATES

# Not exported =================================================================
is_zero_numeric <- function(x, tolerance = sqrt(.Machine$double.eps)) {
  if (is.numeric(x)) return(abs(x) <= tolerance)
  rep(FALSE, length(x))
}

# Attributes ===================================================================
#' Attributes Predicates
#'
#' * `has_length()` checks how long is an object.
#' * `is_empty()` checks is an object is empty (any zero-length dimensions).
#' @param x A [`vector`] to be tested.
#' @param n A length-one [`numeric`] vector specifying the length to test `x`
#'  with. If `NULL`, returns `TRUE` if `x` has length greater than zero, and
#'  `FALSE` otherwise.
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-attributes
#' @rdname predicate-attributes
NULL

#' @export
#' @rdname predicate-attributes
has_length <- function(x, n = NULL) {
  if (is.null(n)) length(x) > 0 else length(x) == n
}

#' @export
#' @rdname predicate-attributes
is_empty <- function(x) {
  if (!is.null(dim(x))) nrow(x) == 0 || ncol(x) == 0
  else length(x) == 0
}

# Names ========================================================================
#' Names Predicates
#'
#' Checks if an object is named.
#' @param x A [`vector`] to be tested.
#' @param names A [`character`] vector specifying the names to test `x`
#'  with. If `NULL`, returns `TRUE` if `x` has names, and `FALSE` otherwise.
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-names
#' @rdname predicate-names
NULL

#' @export
#' @rdname predicate-names
has_names <- function(x, names = NULL) {
  if (is.null(names)) {
    has_length(names(x))
  } else {
    identical(names(x), names)
  }
}

#' @export
#' @rdname predicate-names
has_rownames <- function(x, names = NULL) {
  if (is.null(names)) {
    has_length(rownames(x))
  } else {
    identical(rownames(x), names)
  }
}

#' @export
#' @rdname predicate-names
has_colnames <- function(x, names = NULL) {
  if (is.null(names)) {
    has_length(colnames(x))
  } else {
    identical(colnames(x), names)
  }
}

# NA/NaN/Inf/duplicates ========================================================
#' Utility Predicates
#'
#' * `has_missing()` and `has_infinite()` check if an object contains missing
#' or infinite values.
#' * `has_duplicates()` checks if an object has duplicated elements.
#' @param x A [`vector`] to be tested.
#' @param tolerance A [`numeric`] scalar giving the tolerance to check within
#'  (for `numeric` vector).
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`)
#'  be omitted?
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-data
#' @rdname predicate-data
NULL

#' @export
#' @rdname predicate-data
has_missing <- function(x) {
  any(is.na(x))
}

#' @export
#' @rdname predicate-data
has_infinite <- function(x) {
  any(is.infinite(x))
}

#' @export
#' @rdname predicate-data
has_duplicates <- function(x) {
  any(duplicated(x))
}

is_duplicated <- function(x) {
  duplicated(x, fromLast = FALSE) | duplicated(x, fromLast = TRUE)
}

#' @export
#' @rdname predicate-data
is_unique <- function(x, tolerance = sqrt(.Machine$double.eps), na.rm = FALSE) {
  if (na.rm) x <- stats::na.omit(x)
  if (is.numeric(x)) {
    cte <- is_constant(x, tolerance = tolerance)
    if (is.na(cte)) cte <- FALSE
  } else {
    cte <- length(unique(x)) == 1
  }
  cte
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

#' @export
#' @rdname predicate-type
is_list <- function(x) {
  typeof(x) == "list"
}
#' @export
#' @rdname predicate-type
is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double",
                   "complex", "character", "raw")
}
#' @export
#' @rdname predicate-type
is_vector <- function(x) {
  is_atomic(x) || is_list(x)
}
#' @export
#' @rdname predicate-type
is_numeric <- function(x) {
  typeof(x) %in% c("integer", "double")
}
#' @export
#' @rdname predicate-type
is_integer <- function(x) {
  typeof(x) == "integer"
}
#' @export
#' @rdname predicate-type
is_double <- function(x) {
  typeof(x) == "double"
}
#' @export
#' @rdname predicate-type
is_character <- function(x) {
  typeof(x) == "character"
}
#' @export
#' @rdname predicate-type
is_logical <- function(x) {
  typeof(x) == "logical"
}
#' @export
#' @rdname predicate-type
is_error <- function(x) {
  inherits(x, "try-error") || inherits(x, "simpleError") || inherits(x, "error")
}
#' @export
#' @rdname predicate-type
is_warning <- function(x) {
  inherits(x, "simpleWarning") || inherits(x, "warning")
}
#' @export
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
#' @name is_scalar
#' @rdname is_scalar
NULL

#' @export
#' @rdname is_scalar
is_scalar_list <- function(x) {
  is_list(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
is_scalar_atomic <- function(x) {
  is_atomic(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
is_scalar_vector <- function(x) {
  is_vector(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
is_scalar_numeric <- function(x) {
  is_numeric(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
is_scalar_integer <- function(x) {
  is_integer(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
is_scalar_double <- function(x) {
  is_double(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
is_scalar_character <- function(x) {
  is_character(x) && length(x) == 1
}
#' @export
#' @rdname is_scalar
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
#' @param ... Currently not used.
#' @return A [`logical`] vector.
#' @family predicates
#' @name predicate-numeric
#' @rdname predicate-numeric
NULL

#' @export
#' @rdname predicate-numeric
is_zero <- function(x, tolerance = sqrt(.Machine$double.eps), ...) {
  assert_type(x, "numeric")
  abs(x) <= tolerance
}
#' @export
#' @rdname predicate-numeric
is_odd <- function(x, ...) { # impair
  assert_type(x, "numeric")
  as.logical(x %% 2)
}
#' @export
#' @rdname predicate-numeric
is_even <- function(x, ...) { # pair
  assert_type(x, "numeric")
  !as.logical(x %% 2)
}
#' @export
#' @rdname predicate-numeric
is_positive <- function(x, strict = FALSE, ...) {
  assert_type(x, "numeric")
  if (strict) x > 0 else x >= 0
}
#' @export
#' @rdname predicate-numeric
is_negative <- function(x, strict = FALSE, ...) {
  assert_type(x, "numeric")
  if (strict) x < 0 else x <= 0
}
#' @export
#' @rdname predicate-numeric
is_whole <- function(x, tolerance = sqrt(.Machine$double.eps), ...) {
  assert_type(x, "numeric")
  abs(x - round(x, digits = 0)) <= tolerance
}

#' Numeric Trend Predicates
#'
#' Check numeric objects:
#' * `is_constant()` checks for equality among all elements of a vector.
#' * `is_increasing()` and `is_decreasing()` check if a sequence of numbers
#'   is monotonically increasing or decreasing, respectively.
#' @param x,y A [`numeric`] object to be tested.
#' @param tolerance A [`numeric`] scalar giving the tolerance to check within.
#' @param strict A [`logical`] scalar: should strict inequality be used?
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`)
#'  be omitted?
#' @return A [`logical`] scalar.
#' @family predicates
#' @name predicate-trend
#' @rdname predicate-trend
NULL

#' @export
#' @rdname predicate-trend
is_constant <- function(x, tolerance = sqrt(.Machine$double.eps), na.rm = FALSE) {
  assert_type(x, "numeric")
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
}
#' @export
#' @rdname predicate-trend
is_increasing <- function(x, na.rm = FALSE) {
  assert_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  all(x == cummax(x))
}
#' @export
#' @rdname predicate-trend
is_decreasing <- function(x, na.rm = FALSE) {
  assert_type(x, "numeric")
  if (na.rm) x <- stats::na.omit(x)
  all(x == cummin(x))
}
#' @export
#' @rdname predicate-trend
is_greater <- function(x, y, strict = FALSE, na.rm = FALSE) {
  assert_type(x, "numeric")
  assert_type(y, "numeric")
  z <- if (strict) x > y else x >= y
  all(z, na.rm = na.rm)
}
#' @export
#' @rdname predicate-trend
is_lower <- function(x, y, strict = FALSE, na.rm = FALSE) {
  assert_type(x, "numeric")
  assert_type(y, "numeric")
  z <- if (strict) x < y else x <= y
  all(z, na.rm = na.rm)
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
# Graph Predicates
#
# `is_dag()` checks if a graph has a topological ordering (i.e. is a directed
# acyclic graph) using Kahn's algorithm.
# @param x An adjacency [`matrix`] to be tested.
# @return A [`logical`] scalar.
# @references
#  Kahn, A. B. (1962). Topological sorting of large networks. *Communications
#  of the ACM*, 5(11), p. 558-562. \doi{10.1145/368996.369025}.
# @family predicates
# @keywords internal
# @export
# is_dag <- function(x) {
#   # Get edges
#   G <- matrix2edges(x)
#   # Find nodes which have no incoming edges
#   S <- which(colSums(x) == 0)
#   # List that will contain the sorted elements
#   L <- list()
#
#   if (length(S) == 0)
#     return(FALSE)
#
#   k <- 1L
#   while (k == 1 || length(S) != 0) {
#     # Remove a node n from S and add n to tail of L
#     n <- S[[1]]
#     S <- S[-1]
#     L <- append(L, n)
#     # For each node m with an edge e from n to m
#     e <- which(G[, 1] == n)
#     m <- G[e, 2]
#     # Do remove edge e from the graph
#     G <- G[-e, , drop = FALSE]
#     # If m has no other incoming edges then insert m into S
#     if (nrow(G) != 0) {
#       m <- m[!(m %in% G[, 2])]
#       if (length(m) != 0)
#         S <- append(S, m)
#     }
#     k <- k + 1
#   }
#   return(nrow(G) == 0)
# }

# matrix2edges <- function(from) {
#   edges <- matrix(data = NA, nrow = 0, ncol = 2)
#   nodes <- seq_len(nrow(from))
#   for (i in nodes) {
#     to <- which(from[i, ])
#     if (length(to) != 0) {
#       e <- cbind(form = i, to = to)
#       edges <- rbind(edges, e)
#     }
#   }
#
#   edges
# }
