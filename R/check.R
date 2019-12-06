# CHECK DATA INPUT
#' @include predicates.R
NULL

# ======================================================================== Types
#' Check Data Types
#'
#' @param x An object to be checked.
#' @param expected A \code{\link{character}} string specifying the expected
#' type.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family check
#' @name check-type
#' @rdname check-type
#' @keywords internal
NULL

#' @rdname check-type
check_type <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    list = is_list,
    atomic = is_atomic,
    vector = is_vector,
    numeric = is_numeric,
    integer = is_integer,
    double = is_double,
    character = is_character,
    logical = is_logical,
    stop("Can't find a predicate for this type: ", expected, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf("%s must be %s; not %s.", sQuote(arg), expected, typeof(x))
    throw_error("error_bad_type", msg)
  }
}
#' @rdname check-type
check_scalar <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    list = is_scalar_list,
    atomic = is_scalar_atomic,
    vector = is_scalar_vector,
    numeric = is_scalar_numeric,
    integer = is_scalar_integer,
    double = is_scalar_double,
    character = is_scalar_character,
    logical = is_scalar_logical,
    stop("Can't find a predicate for this scalar: ", expected, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf("%s must be a scalar (%s).", sQuote(arg), expected)
    throw_error("error_bad_scalar", msg)
  }
}

# =================================================================== Attributes
#' Check Object Attributes
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family check
#' @name check-attribute
#' @rdname check-attribute
#' @keywords internal
NULL

#' @rdname check-attribute
check_length <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- length(x)
  if (n != expected) {
    msg <- sprintf("%s must be of length %d; not %s.", sQuote(arg), expected, n)
    throw_error("error_bad_dimension", msg)
  }
}
#' @rdname check-attribute
check_lengths <- function(x, expected = NULL) {
  arg <- deparse(substitute(x))
  n <- lengths(x)
  m <- paste0(n, collapse = ", ")
  if (is.null(expected)) {
    if (!is_equal(n)) {
      msg <- sprintf("Elements of %s must have the same length; not %s.",
                     sQuote(arg), m)
      throw_error("error_bad_dimension", msg)
    }
  } else {
    expected <- as.integer(expected)
    if (is_empty(n) || any(n != expected)) {
      msg <- sprintf("Elements of %s must have the following lengths %s; not %s.",
                     sQuote(arg), paste0(expected, collapse = ", "), m)
      throw_error("error_bad_dimension", msg)
    }
  }
}
#' @rdname check-attribute
check_dimension <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- dim(x)
  expected <- as.integer(expected)
  if (!identical(n, expected)) {
    msg <- sprintf("%s must be of dimension %s; not %s.", sQuote(arg),
                   paste0(expected, collapse = " x "),
                   paste0(n, collapse = " x "))
    throw_error("error_bad_dimension", msg)
  }
}
#' @rdname check-attribute
check_names <- function(x, expected = NULL, margin = c(1, 2)) {
  arg <- deparse(substitute(x))
  if (is.array(x) || is.data.frame(x)) {
    n <- dimnames(x)[[margin]]
    if (is_scalar_numeric(margin)) {
      mar <- ifelse(margin == 1, "row ", "column ")
    } else {
      mar <- "dim"
    }
  } else {
    n <- names(x)
    mar <- ""
  }
  if (is.null(expected)) {
    if (is_empty(n)) {
      msg <- sprintf("%s must have %snames.", sQuote(arg), mar)
      throw_error("error_bad_names", msg)
    }
  } else if (is_empty(n) || !identical(n, expected)) {
    msg <- sprintf("%s must have the following %snames: %s.",
                   sQuote(arg), mar, paste0(expected, collapse = ", "))
    throw_error("error_bad_names", msg)
  }
}
#' @rdname check-attribute
check_uuid <- function(x) {
  arg <- deparse(substitute(x))
  if (length(x) == 0 || !is_uuid(x)) {
    msg <- sprintf("%s must be an UUID.", sQuote(arg))
    throw_error("error_bad_uuid", msg)
  }
  # if (x == "00000000-0000-4000-a000-000000000000") {
  #   msg <- sprintf("%s seems wrong.", sQuote(arg))
  #   warning("error_bad_uuid", msg)
  # }
}

# =================================================================== NA/NaN/Inf
#' Check Missing Values
#'
#' Checks if an object contains any missing (\code{NA}, \code{NaN}) or infinite
#' (\code{Inf}) value.
#' @param x An object to be checked.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family check
#' @name check-missing
#' @rdname check-missing
#' @keywords internal
NULL

#' @rdname check-missing
check_missing <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.na(x))
  if (n > 0) {
    msg <- sprintf("%s must not contain missing values (%d detected).",
                   sQuote(arg), n)
    throw_error("error_data_missing", msg)
  }
}
#' @rdname check-missing
check_infinite <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.infinite(x))
  if (n > 0) {
    msg <- sprintf("%s must not contain infinite values (%d detected).",
                   sQuote(arg), n)
    throw_error("error_data_infinite", msg)
  }
}

# ===================================================================== Numerric
#' Check Numeric Values
#'
#' @param x A \code{\link{numeric}} object to be checked.
#' @param expected An appropriate expected value.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family check
#' @name check-numeric
#' @rdname check-numeric
#' @keywords internal
NULL

#' @rdname check-numeric
check_numbers <- function(x, expected = c("positive", "whole", "odd", "even"),
                          ...) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    positive = is_positive,
    whole = is_whole,
    odd = is_odd,
    even = is_even,
    stop("Can't find a predicate for this: ", expected, call. = FALSE)
  )
  if (!all(predicate(x, ...))) {
    msg <- sprintf("%s must contain %s numbers.", sQuote(arg), expected)
    throw_error("error_bad_number", msg)
  }
}
#' @rdname check-numeric
check_constant <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_equal(x)) {
    msg <- sprintf("%s must be constant.", sQuote(arg))
    throw_error("error_bad_value", msg)
  }
}

# ======================================================================= Matrix
#' Check Matrix
#'
#' @param x A \code{\link{matrix}} to be checked.
#' @param expected An appropriate expected value.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family check
#' @name check-matrix
#' @rdname check-matrix
#' @keywords internal
NULL

#' @rdname check-matrix
check_square <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_square(x)) {
    k <- paste0(dim(x), collapse = " x ")
    msg <- sprintf("%s must be a square matrix, not %s.", sQuote(arg), k)
    throw_error("error_bad_matrix", msg)
  }
}
#' @rdname check-matrix
check_symmetric <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_symmetric(x)) {
    msg <- sprintf("%s must be a symmetric matrix.", sQuote(arg))
    throw_error("error_bad_matrix", msg)
  }
}

# ======================================================================== Graph
#' Check Graph
#'
#' @param x A \code{\link{matrix}} to be checked.
#' @param expected An appropriate expected value.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family check
#' @name check-graph
#' @rdname check-graph
#' @keywords internal
NULL

#' @rdname check-graph
check_dag <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_dag(x)) {
    msg <- sprintf("%s must not contain cycles.", sQuote(arg))
    throw_error("error_bad_graph", msg)
  }
}
