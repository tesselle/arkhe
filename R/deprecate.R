# DEPRECATED

#' @rdname deprecate
#' @aliases as_long-method
setGeneric(
  name = "as_long",
  def = function(from, ...) standardGeneric("as_long"),
  valueClass = "data.frame"
)

#' @export
#' @rdname deprecate
#' @aliases as_long,matrix-method
setMethod(
  f = "as_long",
  signature = signature(from = "matrix"),
  definition = function(from, factor = FALSE, reverse = FALSE) {
    .Deprecated("to_long()", old = "as_long()")
    to_long(from, factor = FALSE, reverse = FALSE)
  }
)

#' @rdname deprecate
#' @aliases confidence-method
setGeneric(
  name = "confidence",
  def = function(object, ...) standardGeneric("confidence")
)

#' @export
#' @rdname deprecate
#' @aliases confidence,numeric-method
setMethod(
  f = "confidence",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95, type = c("student", "normal")) {
    .Deprecated("confidence_mean()", old = "confidence()")
    confidence_mean(object, level = level, type = type)
  }
)

#' @export
#' @rdname deprecate
assert_numeric <- function(x, expected, ...) {
  .Deprecated()
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
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname deprecate
assert_trend <- function(x, expected, ...) {
  .Deprecated("assert_constant(), assert_decreasing() or assert_increasing()")
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
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname deprecate
assert_relation <- function(x, y, expected, ...) {
  .Deprecated("assert_lower() or assert_greater()")
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
    throw_error("error_bad_numeric", msg)
  }
  invisible(x)
}

#' @export
#' @rdname deprecate
assert_matrix <- function(x, expected) {
  .Deprecated("assert_square() or assert_symmetric()")
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

#' @export
#' @rdname deprecate
remove_empty <- function(x, margin = 1) {
  .Deprecated("compact()")
  vide <- function(x) {
    miss <- is.na(x)
    if (is_numeric(x)) x == 0 | miss
    else if (is_character(x)) x == "" | miss
    else miss
  }
  discard(x, f = vide, margin = margin, all = TRUE)
}
