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

# Packages =====================================================================
#' Check the Availability of a Package
#'
#' @param x A [`character`] vector naming the packages to check.
#' @param ask A [`logical`] scalar: should the user be asked to select packages
#'  before they are downloaded and installed?
#' @details
#'  `assert_package()` is designed for use inside other functions in your own
#'  package to check for the availability of a suggested package.
#'
#'  If the required packages are not available and \R is running interactively,
#'  the user will be asked to install the packages.
#'
#'  `needs()` is an alias for `assert_package()`.
#' @return Invisibly returns `NULL`.
#' @family validation methods
#' @author N. Frerebeau
#' @export
assert_package <- function(x, ask = TRUE) {
  ok <- vapply(X = x, FUN = requireNamespace, FUN.VALUE = logical(1),
               quietly = TRUE)

  miss <- x[!ok]
  n <- length(miss)

  if (n != 0) {
    msg <- ngettext(n, "Package %s is required.", "Packages %s are required.")
    pkg <- paste0(sQuote(miss), collapse = ", ")
    err <- sprintf(msg, pkg)
    install <- "0"
    if (ask && interactive()) {
      cat(
        err,
        sprintf("Do you want to install %s?", ngettext(n, "it", "them")),
        "1. Yes",
        "2. No",
        sep = "\n"
      )
      install <- readline("Choice: ")
    }
    if (install == "1") {
      utils::install.packages(miss)
    } else {
      throw_error("error_missing_package", err)
    }
  }
  invisible(NULL)
}

#' @export
#' @rdname assert_package
needs <- assert_package

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
#' @export
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
#' @rdname assert_type
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

#' @export
#' @rdname assert_type
assert_function <- function(x) {
  arg <- deparse(substitute(x))
  if (!is.function(x)) {
    msg <- sprintf("%s must be a function.", sQuote(arg))
    throw_error("error_bad_type", msg)
  }
  invisible(x)
}

# Attributes ===================================================================
#' Check Object Length/Dimensions
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @param empty A [`logical`] scalar: should empty objects be ignored?
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @export
assert_length <- function(x, expected, empty = FALSE) {
  arg <- deparse(substitute(x))
  if (!(empty & is_empty(x)) && !has_length(x, n = expected)) {
    msg <- sprintf("%s must be of length %d; not %d.", sQuote(arg),
                   expected, length(x))
    throw_error("error_bad_length", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_length
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
#' @rdname assert_length
assert_empty <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_empty(x)) {
    msg <- sprintf("%s must be empty.", sQuote(arg))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_length
assert_filled <- function(x) {
  arg <- deparse(substitute(x))
  if (is_empty(x)) {
    msg <- sprintf("%s must not be empty.", sQuote(arg))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_length
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

#' Check Object Names
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @export
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
#' @rdname assert_names
assert_dimnames <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (!identical(dimnames(x), expected)) {
    msg <- sprintf("%s must have dimnames.", sQuote(arg))
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_names
assert_rownames <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (!identical(rownames(x), expected)) {
    msg <- sprintf("%s must have rownames.", sQuote(arg))
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_names
assert_colnames <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (!identical(colnames(x), expected)) {
    msg <- sprintf("%s must have rownames.", sQuote(arg))
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

# NA/NaN/Inf/duplicates ========================================================
#' Check Data
#'
#' * `assert_missing()` and `assert_infinite()` check if an object contains any
#' missing (`NA`, `NaN`) or infinite (`Inf`) value.
#' * `assert_unique()` checks if an object contains duplicated elements.
#' @param x An object to be checked.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @name assert_data
#' @rdname assert_data
NULL

#' @export
#' @rdname assert_data
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
#' @rdname assert_data
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
#' @rdname assert_data
assert_unique <- function(x) {
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
#' @param x A [`numeric`] object to be checked.
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`)
#'  be omitted?
#' @param ... Extra parameters to be passed to internal methods.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @name assert_numeric
#' @rdname assert_numeric
NULL

#' @export
#' @rdname assert_numeric
assert_count <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_whole(x, ...), na.rm = na.rm)) {
    msg <- sprintf("%s must contain integers (counts).", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_whole <- assert_count

#' @export
#' @rdname assert_numeric
assert_positive <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_positive(x, ...), na.rm = na.rm)) {
    msg <- sprintf("%s must contain positive numbers.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_negative <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_negative(x, ...), na.rm = na.rm)) {
    msg <- sprintf("%s must contain negative numbers.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_odd <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_odd(x, ...), na.rm = na.rm)) {
    msg <- sprintf("%s must contain odd numbers.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_even <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_even(x, ...), na.rm = na.rm)) {
    msg <- sprintf("%s must contain even numbers.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' Check Numeric Trend
#'
#' @param x A [`numeric`] object to be checked.
#' @param ... Extra parameters to be passed to internal methods.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @export
assert_constant <- function(x, ...) {
  arg <- deparse(substitute(x))
  if (!is_constant(x, ...)) {
    msg <- sprintf("%s must be constant.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_constant
assert_decreasing <- function(x, ...) {
  arg <- deparse(substitute(x))
  if (!is_decreasing(x, ...)) {
    msg <- sprintf("%s must be decreasing.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_constant
assert_increasing <- function(x, ...) {
  arg <- deparse(substitute(x))
  if (!is_increasing(x, ...)) {
    msg <- sprintf("%s must be increasing.", sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' Check Numeric Relations
#'
#' @param x,y A [`numeric`] object to be checked.
#' @param ... Extra parameters to be passed to internal methods.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @export
assert_lower <- function(x, y, ...) {
  arg_x <- deparse(substitute(x))
  arg_y <- deparse(substitute(y))
  if (!is_lower(x, y, ...)) {
    msg <- sprintf("%s must be lower than %s.", sQuote(arg_x), sQuote(arg_y))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_lower
assert_greater <- function(x, y, ...) {
  arg_x <- deparse(substitute(x))
  arg_y <- deparse(substitute(y))
  if (!is_greater(x, y, ...)) {
    msg <- sprintf("%s must be greater than %s.", sQuote(arg_x), sQuote(arg_y))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

# Matrix =======================================================================
#' Check Matrix
#'
#' @param x A [`matrix`] to be checked.
#' @return Throw an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family validation methods
#' @export
assert_square <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_square(x)) {
    k <- paste0(dim(x), collapse = " x ")
    msg <- sprintf("%s must be a square matrix, not %s.", sQuote(arg), k)
    throw_error("error_bad_matrix", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_square
assert_symmetric <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_symmetric(x)) {
    msg <- sprintf("%s must be a symmetric matrix.", sQuote(arg))
    throw_error("error_bad_matrix", msg)
  }
  invisible(x)
}

# Graph ========================================================================
# Check Graph
#
# @param x A [`matrix`] to be checked.
# @param expected An appropriate expected value.
# @return Throw an error, if any.
# @author N. Frerebeau
# @family validation methods
# @keywords internal
# @export
# assert_dag <- function(x) {
#   arg <- deparse(substitute(x))
#   if (!is_dag(x)) {
#     msg <- sprintf("%s must not contain cycles.", sQuote(arg))
#     throw_error("error_bad_graph", msg)
#   }
#   invisible(x)
# }
