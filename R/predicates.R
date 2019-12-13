# PREDICATES

#' Utility Predicates
#'
#' \code{is_empty} checks if a vector or list is empty.
#'
#' \code{is_named} checks if an object is named.
#'
#' \code{is_uuid} checks if a string is a canonically formatted UUID that is
#' Version 1 through 5 and is the appropriate Variant as per RFC4122.
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @family predicates
#' @name predicate-utils
#' @rdname predicate-utils
#' @keywords internal
NULL

#' @export
#' @rdname predicate-utils
is_empty <- function(x) {
  length(x) == 0
}
#' @export
#' @rdname predicate-utils
is_named <- function(x) {
  length(names(x)) != 0
}
#' @export
#' @rdname predicate-utils
is_uuid <- function(x) {
  pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(pattern, x)
}

# ==============================================================================
#' Type Predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @family predicates
#' @name predicate-type
#' @rdname predicate-type
#' @keywords internal
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

# ==============================================================================
#' Scalar Type Predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
#' @family predicates
#' @name predicate-scalar
#' @rdname predicate-scalar
#' @keywords internal
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

# ==============================================================================
#' Numeric Predicates
#'
#' Check numeric objects:
#'
#' \code{is_odd} and \code{is_even} check if a number is odd or even,
#' respectively.
#'
#' \code{is_positive} checks if an object contains only (strictly) positive
#' numbers.
#'
#' \code{is_binary} checks if an object contains only \eqn{0}s and \eqn{1}s.
#'
#' \code{is_whole} checks if an object only contains whole numbers.
#' @param x A \code{\link{numeric}} object to be tested.
#' @param tolerance A \code{\link{numeric}} scalar giving the
#'  tolerance to check within.
#' @param strict A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted?
#' @return A \code{\link{logical}} vector.
#' @family predicates
#' @name predicate-numeric
#' @rdname predicate-numeric
#' @keywords internal
NULL

#' @export
#' @rdname predicate-numeric
is_odd <- function(x) { # impair
  check_type(x, "numeric")
  as.logical(x %% 2)
}
#' @export
#' @rdname predicate-numeric
is_even <- function(x) { # pair
  check_type(x, "numeric")
  !as.logical(x %% 2)
}
#' @export
#' @rdname predicate-numeric
is_positive <- function(x, strict = FALSE) {
  check_type(x, "numeric")
  if (strict) x > 0 else x >= 0
}
#' @export
#' @rdname predicate-numeric
is_whole <- function(x, tolerance = .Machine$double.eps^0.5) {
  check_type(x, "numeric")
  abs(x - round(x, digits = 0)) <= tolerance
}
#' @export
#' @rdname predicate-numeric
is_binary <- function(x) {
  check_type(x, "numeric")
  x %in% c(0, 1)
}

#' Numeric Trend Predicates
#'
#' Check numeric objects:
#'
#' \code{is_equal} checks for equality among all elements of a vector.
#'
#' \code{is_increasing} and \code{is_decreasing} check if a sequence of numbers
#' is monotonically increasing or decreasing, respectively.
#'
#' \code{is_overlapping} checks if two data ranges overlap at all.
#' @param x,y A \code{\link{numeric}} object to be tested.
#' @param tolerance A \code{\link{numeric}} scalar giving the
#'  tolerance to check within.
#' @param na.rm A \code{\link{logical}} scalar: should missing values
#'  (including \code{NaN}) be omitted?
#' @return A \code{\link{logical}} scalar.
#' @family predicates
#' @name predicate-trend
#' @rdname predicate-trend
#' @keywords internal
NULL

#' @export
#' @rdname predicate-trend
is_equal <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
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
#' @export
#' @rdname predicate-trend
is_overlapping <- function(x, y) {
  check_type(x, "numeric")
  check_type(y, "numeric")

  min(x) <= max(y) && max(x) >= min(y)
}

# ==============================================================================
#' Matrix Predicates
#'
#' \code{is_square} checks if a matrix is square.
#'
#' \code{is_symmetric} checks if a matrix is symmetric.
#' @param x A \code{\link{matrix}} to be tested.
#' @return A \code{\link{logical}} scalar.
#' @family predicates
#' @name predicate-matrix
#' @rdname predicate-matrix
#' @keywords internal
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

# ==============================================================================
#' Graph Predicates
#'
#' \code{is_dag} checks if a graph has a topological ordering (i.e. is a
#' directed acyclic graph) using Kahn's algorithm.
#' @param x An adjacency \code{\link{matrix}} to be tested.
#' @return A \code{\link{logical}} scalar.
#' @references
#'  Kahn, A. B. (1962). Topological sorting of large networks.
#'  \emph{Communications of the ACM}, 5(11), p. 558-562.
#'  DOI: \href{https://doi.org/10.1145/368996.369025}{10.1145/368996.369025}.
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
