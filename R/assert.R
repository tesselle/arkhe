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
#' @return Invisibly returns `NULL`.
#' @family checking methods
#' @author N. Frerebeau
#' @export
assert_package <- function(x, ask = interactive()) {
  ok <- vapply(X = x, FUN = requireNamespace, FUN.VALUE = logical(1),
               quietly = TRUE)

  miss <- x[!ok]
  n <- length(miss)

  if (n > 0) {
    err <- sprintf(
      ngettext(n, "Package %s is required.", "Packages %s are required."),
      paste0(sQuote(miss), collapse = ", ")
    )
    install <- FALSE
    if (isTRUE(ask)) {
      msg <- ngettext(n, "Do you want to install it?", "Do you want to install them?")
      install <- utils::askYesNo(
        msg = paste0(c(err, msg), collapse = "\n"),
        default = FALSE,
        prompts = gettext(c("Yes", "No", "Cancel"))
      )
    }
    if (isTRUE(install)) {
      utils::install.packages(miss)
    } else {
      throw_error("error_missing_package", err)
    }
  }
  invisible(NULL)
}

# Attributes ===================================================================
#' Check Object Length(s)
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @param allow_empty A [`logical`] scalar: should [empty][is_empty()] object be
#'  ignored?
#' @param allow_null A [`logical`] scalar: should `NULL` object be ignored?
#' @param empty Deprecated.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_length <- function(x, expected, allow_empty = empty, allow_null = FALSE, empty = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) return(invisible(NULL))

  arg <- deparse(substitute(x))
  if (!(allow_empty && is_empty(x)) && !has_length(x, n = expected)) {
    txt <- tr_("%s must be of length %d; not %d.")
    msg <- sprintf(txt, sQuote(arg), expected, length(x))
    throw_error("error_bad_length", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_length
assert_lengths <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- lengths(x)
  if (!all(n == expected)) {
    txt <- tr_("Elements of %s must be of lengths %s; not %s.")
    msg <- sprintf(txt, sQuote(arg), paste0(expected, collapse = ", "),
                   paste0(n, collapse = ", "))
    throw_error("error_bad_length", msg)
  }
  invisible(x)
}

#' Check Object Dimensions
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_dim <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- dim(x)
  if (!all(n == expected)) {
    txt <- tr_("%s must be of dimension %s; not %s.")
    msg <- sprintf(txt, sQuote(arg), paste0(expected, collapse = " x "),
                   paste0(n, collapse = " x "))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_dim
assert_nrow <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- nrow(x)
  if (n != expected) {
    txt <- ngettext(expected, "%s must have %s row; not %s.",
                    "%s must have %s rows; not %s.")
    msg <- sprintf(txt, sQuote(arg), expected, n)
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_dim
assert_ncol <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- ncol(x)
  if (n != expected) {
    txt <- ngettext(expected, "%s must have %s column; not %s.",
                    "%s must have %s columns; not %s.")
    msg <- sprintf(txt, sQuote(arg), expected, n)
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' Check Object Filling
#'
#' Checks if an object is (not) empty.
#' @param x An object to be checked.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_empty <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_empty(x)) {
    msg <- sprintf(tr_("%s must be empty."), sQuote(arg))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_empty
assert_filled <- function(x) {
  arg <- deparse(substitute(x))
  if (is_empty(x)) {
    msg <- sprintf(tr_("%s must not be empty."), sQuote(arg))
    throw_error("error_bad_dimensions", msg)
  }
  invisible(x)
}

# Names ========================================================================
#' Check Object Names
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_names <- function(x, expected = NULL) {
  arg <- deparse(substitute(x))
  if (!has_names(x, names = expected)) {
    if (is.null(expected)) {
      msg <- sprintf(tr_("%s must have names."), sQuote(arg))
    } else {
      msg <- sprintf(tr_("%s must have the following names: %s."),
                     sQuote(arg), paste0(expected, collapse = ", "))
    }
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_names
assert_rownames <- function(x, expected = NULL) {
  arg <- deparse(substitute(x))
  if (!has_rownames(x, names = expected)) {
    if (is.null(expected)) {
      msg <- sprintf(tr_("%s must have row names."), sQuote(arg))
    } else {
      msg <- sprintf(tr_("%s must have the following row names: %s."),
                     sQuote(arg), paste0(expected, collapse = ", "))
    }
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_names
assert_colnames <- function(x, expected = NULL) {
  arg <- deparse(substitute(x))
  if (!has_colnames(x, names = expected)) {
    if (is.null(expected)) {
      msg <- sprintf(tr_("%s must have column names."), sQuote(arg))
    } else {
      msg <- sprintf(tr_("%s must have the following column names: %s."),
                     sQuote(arg), paste0(expected, collapse = ", "))
    }
    throw_error("error_bad_names", msg)
  }
  invisible(x)
}

# NA/NaN/Inf/duplicates ========================================================
#' Check Missing Values
#'
#' Checks if an object contains any missing (`NA`, `NaN`) values.
#' @param x An object to be checked.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_missing <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.na(x))
  if (n > 0) {
    txt <- ngettext(n, "%s must not contain missing values (%d detected).",
                    "%s must not contain missing values (%d detected).")
    msg <- sprintf(txt, sQuote(arg), n)
    throw_error("error_data_missing", msg)
  }
  invisible(x)
}

#' Check Infinite Values
#'
#' Checks if an object contains any infinite (`Inf`) values.
#' @param x An object to be checked.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_infinite <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.infinite(x))
  if (n > 0) {
    txt <- ngettext(n, "%s must not contain infinite values (%d detected).",
                    "%s must not contain infinite values (%d detected).")
    msg <- sprintf(txt, sQuote(arg), n)
    throw_error("error_data_infinite", msg)
  }
  invisible(x)
}

#' Check Duplicates
#'
#' Checks if an object contains duplicated elements.
#' @param x An object to be checked.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_unique <- function(x) {
  arg <- deparse(substitute(x))
  if (has_duplicates(x)) {
    msg <- sprintf(tr_("Elements of %s must be unique."), sQuote(arg))
    throw_error("error_data_duplicates", msg)
  }
  invisible(x)
}

# Types ========================================================================
#' Check Data Types
#'
#' @param x An object to be checked.
#' @param expected A [`character`] string specifying the expected
#'  type. It must be one of "`list`", "`atomic`", "`vector`", "`numeric`",
#'  "`integer`", "`double`", "`character`" or "`logical`".
#' @param allow_empty A [`logical`] scalar: should [empty][is_empty()] object be
#'  allowed?
#' @param allow_null A [`logical`] scalar: should `NULL` object be ignored?
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @author N. Frerebeau
#' @family checking methods
#' @export
assert_type <- function(x, expected, allow_empty = TRUE, allow_null = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) return(invisible(NULL))
  if (isFALSE(allow_empty)) assert_filled(x)

  arg <- deparse(substitute(x))
  msg <- sprintf(tr_("Can't find a predicate for this type: %s."), expected)
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
    stop(msg, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf(tr_("%s must be %s; not %s."), sQuote(arg), expected, typeof(x))
    throw_error("error_bad_type", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_type
assert_scalar <- function(x, expected) {
  arg <- deparse(substitute(x))
  msg <- sprintf(tr_("Can't find a predicate for this scalar: %s."), expected)
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
    stop(msg, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf(tr_("%s must be a scalar (%s)."), sQuote(arg), expected)
    throw_error("error_bad_scalar", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_type
assert_function <- function(x) {
  arg <- deparse(substitute(x))
  if (!is.function(x)) {
    msg <- sprintf(tr_("%s must be a function."), sQuote(arg))
    throw_error("error_bad_type", msg)
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
#' @family checking methods
#' @name assert_numeric
#' @rdname assert_numeric
NULL

#' @export
#' @rdname assert_numeric
assert_count <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_whole(x, ...), na.rm = na.rm)) {
    msg <- sprintf(tr_("%s must contain integers (counts)."), sQuote(arg))
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
    msg <- sprintf(tr_("%s must contain positive numbers."), sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_negative <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_negative(x, ...), na.rm = na.rm)) {
    msg <- sprintf(tr_("%s must contain negative numbers."), sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_odd <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_odd(x, ...), na.rm = na.rm)) {
    msg <- sprintf(tr_("%s must contain odd numbers."), sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_numeric
assert_even <- function(x, na.rm = FALSE, ...) {
  arg <- deparse(substitute(x))
  if (!all(is_even(x, ...), na.rm = na.rm)) {
    msg <- sprintf(tr_("%s must contain even numbers."), sQuote(arg))
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
#' @family checking methods
#' @export
assert_constant <- function(x, ...) {
  arg <- deparse(substitute(x))
  if (!is_constant(x, ...)) {
    msg <- sprintf(tr_("%s must be constant."), sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_constant
assert_decreasing <- function(x, ...) {
  arg <- deparse(substitute(x))
  if (!is_decreasing(x, ...)) {
    msg <- sprintf(tr_("%s must be decreasing."), sQuote(arg))
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_constant
assert_increasing <- function(x, ...) {
  arg <- deparse(substitute(x))
  if (!is_increasing(x, ...)) {
    msg <- sprintf(tr_("%s must be increasing."), sQuote(arg))
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
#' @family checking methods
#' @export
assert_lower <- function(x, y, ...) {
  arg_x <- deparse(substitute(x))
  arg_y <- deparse(substitute(y))
  if (!is_lower(x, y, ...)) {
    txt <- tr_("%s must be lower than %s.")
    msg <- sprintf(txt, sQuote(arg_x), sQuote(arg_y))
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
    txt <- tr_("%s must be greater than %s.")
    msg <- sprintf(txt, sQuote(arg_x), sQuote(arg_y))
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
#' @family checking methods
#' @export
assert_square <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_square(x)) {
    k <- paste0(dim(x), collapse = " x ")
    msg <- sprintf(tr_("%s must be a square matrix, not %s."), sQuote(arg), k)
    throw_error("error_bad_matrix", msg)
  }
  invisible(x)
}

#' @export
#' @rdname assert_square
assert_symmetric <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_symmetric(x)) {
    msg <- sprintf(tr_("%s must be a symmetric matrix."), sQuote(arg))
    throw_error("error_bad_matrix", msg)
  }
  invisible(x)
}

# Check Graph
#
# @param x A [`matrix`] to be checked.
# @param expected An appropriate expected value.
# @return Throw an error, if any.
# @author N. Frerebeau
# @family checking methods
# @keywords internal
# @export
# assert_dag <- function(x) {
#   arg <- deparse(substitute(x))
#   if (!is_dag(x)) {
#     msg <- sprintf(tr_("%s must not contain cycles."), sQuote(arg))
#     throw_error("error_bad_graph", msg)
#   }
#   invisible(x)
# }
