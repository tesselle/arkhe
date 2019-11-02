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
#' @keywords internal
#' @family predicates
#' @name predicate-utils
#' @rdname predicate-utils
NULL

#' @rdname predicate-utils
is_empty <- function(x) {
  length(x) == 0
}
#' @rdname predicate-utils
is_named <- function(x) {
  length(names(x)) != 0
}
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
  inherits(x, "try-error") || inherits(x, "error")
}

# ==============================================================================
#' Scalar Type Predicates
#'
#' @param x An object to be tested.
#' @return A \code{\link{logical}} scalar.
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

# ==============================================================================
#' Numeric Predicates
#'
#' Check numeric objects:
#'
#' \code{is_odd} and \code{is_even} check if a number is odd or even,
#' respectively.
#'
#' \code{is_positive} checks if an obejct contains only (strictly) positive
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
NULL

#' @rdname predicate-numeric
is_odd <- function(x) { # impair
  check_type(x, "numeric")
  as.logical(x %% 2)
}
#' @rdname predicate-numeric
is_even <- function(x) { # pair
  check_type(x, "numeric")
  !as.logical(x %% 2)
}
#' @rdname predicate-numeric
is_positive <- function(x, strict = FALSE) {
  check_type(x, "numeric")
  if (strict) x > 0 else x >= 0
}
#' @rdname predicate-numeric
is_whole <- function(x, tolerance = .Machine$double.eps^0.5) {
  check_type(x, "numeric")
  abs(x - round(x, digits = 0)) <= tolerance
}
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
NULL

#' @rdname predicate-trend
is_equal <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  check_type(x, "numeric")

  k <- abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
  if (is.na(k)) k <- FALSE
  k
}
#' @rdname predicate-trend
is_increasing <- function(x, na.rm = TRUE) {
  check_type(x, "numeric")

  k <- all(x == cummax(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
#' @rdname predicate-trend
is_decreasing <- function(x, na.rm = TRUE) {
  check_type(x, "numeric")

  k <- all(x == cummin(x), na.rm = na.rm)
  if (is.na(k)) k <- FALSE
  k
}
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
NULL

#' @rdname predicate-matrix
is_square <- function(x) {
  if (is.matrix(x)) nrow(x) == ncol(x) else FALSE
}
#' @rdname predicate-matrix
is_symmetric <- function(x) {
  if (is.matrix(x)) identical(x, t(x)) else FALSE
}
