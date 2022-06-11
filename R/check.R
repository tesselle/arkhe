# CHECK DATA INPUT
#' @include predicates.R
NULL

#' Validate a Condition
#'
#' @param expr An object to be evaluated.
#' @return
#'  Returns `NULL` on success, otherwise returns the error as a string.
#' @author N. Frerebeau
#' @family validation methods
#' @name validate
#' @rdname validate
#' @export
validate <- function(expr) {
  cnd <- catch_message(eval(expr))
  if (has_length(cnd)) return(cnd)
  NULL
}

# Types ========================================================================
#' Check Data Types
#'
#' @param x An object to be checked.
#' @param expected A [`character`] string specifying the expected
#'  type. It must be one of "`list`", "`atomic`", "`vector`", "`numeric`",
#'  "`integer`", "`double`", "`character`" or "`logical`".
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @name check-type
#' @rdname check-type
NULL

#' @export
#' @rdname check-type
assert_type <- function(x, expected) {
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
  invisible(x)
}

#' @export
#' @rdname check-type
assert_scalar <- function(x, expected) {
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
  invisible(x)
}

# Attributes ===================================================================
#' Check Object Attributes
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @param empty A [`logical`] scalar: should empty objects be ignored?
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @name check-attribute
#' @rdname check-attribute
NULL

#' @export
#' @rdname check-attribute
assert_empty <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_empty(x)) {
    msg <- sprintf("%s must be empty.", sQuote(arg))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-attribute
assert_filled <- function(x) {
  arg <- deparse(substitute(x))
  if (is_empty(x)) {
    msg <- sprintf("%s must not be empty.", sQuote(arg))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-attribute
assert_length <- function(x, expected, empty = FALSE) {
  arg <- deparse(substitute(x))
  if (!(empty & is_empty(x)) && !has_length(x, n = expected)) {
    msg <- sprintf("%s must be of length %d; not %s.", sQuote(arg),
                   expected, length(x))
    throw_error("error_bad_length", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-attribute
assert_lengths <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- lengths(x)
  if (any(n != expected)) {
    msg <- sprintf("Elements of %s must be of lengths %s; not %s.", sQuote(arg),
                   paste0(expected, collapse = ", "),
                   paste0(n, collapse = ", "))
    throw_error("error_bad_length", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-attribute
assert_dimensions <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- dim(x)
  if (any(n != expected)) {
    msg <- sprintf("%s must be of dimension %s; not %s.", sQuote(arg),
                   paste0(expected, collapse = " x "),
                   paste0(n, collapse = " x "))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-attribute
assert_names <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (!has_names(x, names = expected)) {
    if (is.null(expected)) {
      msg <- sprintf("%s must have names.", sQuote(arg))
    } else {
      msg <- sprintf("%s must have the following names: %s.",
                     sQuote(arg), paste0(expected, collapse = ", "))
    }
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-attribute
assert_dimnames <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (!identical(dimnames(x), expected)) {
    msg <- sprintf("%s must have dimnames.", sQuote(arg))
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

# NA/NaN/Inf/duplicates ========================================================
#' Check Data
#'
#' * `assert_missing()` and `assert_infinite()` check if an object contains any
#' missing (`NA`, `NaN`) or infinite (`Inf`) value.
#' * `assert_unique()` checks if an object contains duplicated elements.``
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @name check-data
#' @rdname check-data
NULL

#' @export
#' @rdname check-data
assert_missing <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.na(x))
  if (n > 0) {
    msg <- sprintf("%s must not contain missing values (%d detected).",
                   sQuote(arg), n)
    throw_error("error_data_missing", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-data
assert_infinite <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.infinite(x))
  if (n > 0) {
    msg <- sprintf("%s must not contain infinite values (%d detected).",
                   sQuote(arg), n)
    throw_error("error_data_infinite", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-data
assert_unique <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (has_duplicates(x)) {
    msg <- sprintf("Elements of %s must be unique.", sQuote(arg))
    throw_error("error_data_duplicates", msg)
  }
  invisible(x)
}

# Numeric ======================================================================
#' Check Numeric Values
#'
#' @param x,y A [`numeric`] object to be checked.
#' @param expected A [`character`] string specifying the expected
#'  value (see details).
#' @param ... Extra parameters to be passed to internal methods.
#' @details
#'  Possible values for `expected`:
#'  \describe{
#'   \item{`assert_numeric()`}{"`positive`", "`whole`", "`odd`" or "`even`"}
#'   \item{`assert_trend()`}{"`constant`", "`decreasing`" or "`increasing`"}
#'   \item{`assert_relation()`}{"`lower`" or "`greater`"}
#'  }
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @name check-numeric
#' @rdname check-numeric
NULL

#' @export
#' @rdname check-numeric
assert_count <- function(x) {
  arg <- deparse(substitute(x))
  if (!all(x == round(x))) {
    msg <- sprintf("%s must contain integers (counts).", sQuote(arg))
    throw_error("error_bad_number", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-numeric
assert_numeric <- function(x, expected, ...) {
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
  invisible(x)
}

#' @export
#' @rdname check-numeric
assert_trend <- function(x, expected, ...) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    constant = is_constant,
    decreasing = is_decreasing,
    increasing = is_increasing,
    stop("Can't find a predicate for this: ", expected, call. = FALSE)
  )
  if (!predicate(x, ...)) {
    msg <- sprintf("%s must be %s.", sQuote(arg), expected)
    throw_error("error_bad_number", msg)
  }
  invisible(x)
}

#' @export
#' @rdname check-numeric
assert_relation <- function(x, y, expected, ...) {
  arg_x <- deparse(substitute(x))
  arg_y <- deparse(substitute(y))
  predicate <- switch(
    expected,
    lower = is_lower,
    greater = is_greater,
    stop("Can't find a predicate for this: ", expected, call. = FALSE)
  )
  if (!predicate(x, y, ...)) {
    msg <- sprintf("%s must be %s than %s.", sQuote(arg_x), expected,
                   sQuote(arg_y))
    throw_error("error_bad_number", msg)
  }
  invisible(x)
}

# Matrix =======================================================================
#' Check Matrix
#'
#' @param x A [`matrix`] to be checked.
#' @param expected A [`character`] string specifying the expected
#'  value. It must be one of "`square`" or "`symmetric`".
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family validation methods
#' @name check-matrix
#' @rdname check-matrix
NULL

#' @export
#' @rdname check-matrix
assert_matrix <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    square = is_square,
    symmetric = is_symmetric,
    stop("Can't find a predicate for this: ", expected, call. = FALSE)
  )
  if (!predicate(x)) {
    k <- paste0(dim(x), collapse = " x ")
    msg <- sprintf("%s must be a %s matrix, not %s.", sQuote(arg), expected, k)
    throw_error("error_bad_matrix", msg)
  }
  invisible(x)
}

# Graph ========================================================================
#' Check Graph
#'
#' @param x A [`matrix`] to be checked.
#' @param expected An appropriate expected value.
#' @return Throw an error, if any.
#' @author N. Frerebeau
#' @family validation methods
#' @name check-graph
#' @rdname check-graph
#' @keywords internal
NULL

#' @export
#' @rdname check-graph
assert_dag <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_dag(x)) {
    msg <- sprintf("%s must not contain cycles.", sQuote(arg))
    throw_error("error_bad_graph", msg)
  }
  invisible(x)
}
