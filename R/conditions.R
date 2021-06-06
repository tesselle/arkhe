# CONDITIONS
#' @include check.R
NULL

#' Conditions
#'
#' * `throw_error()` stops execution of the current expression and executes an
#'  error action.
#' * `throw_warning()` generates a warning message.
#' * `catch_conditions()` and `catch_message()` handles unusual conditions.
#' @param expr An expression to be evaluated.
#' @param .subclass A [`character`] string specifying the class of
#'  the message to be returned.
#' @param message A [`character`] string specifying the message to be
#'  returned.
#' @param call The call.
#' @param ... Extra arguments.
#' @author N. Frerebeau
#' @name conditions
#' @rdname conditions
#' @family condition
#' @keywords internal error
NULL

#' @rdname conditions
throw_error <- function(.subclass, message, call = NULL, ...) {
  # TODO: gettext
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

#' @rdname conditions
throw_warning <- function(.subclass, message, call = NULL, ...) {
  wrn <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "warning", "condition")
  )
  warning(wrn)
}

#' @rdname conditions
catch_message <- function(expr) {
  cnd <- vector(mode = "character")
  add_msg <- function(x) {
    cnd <<- append(cnd, x$message)
    invokeRestart("muffleMessage")
  }
  add_wrn <- function(x) {
    cnd <<- append(cnd, x$message)
    invokeRestart("muffleWarning")
  }
  add_err <- function(x) {
    cnd <<- append(cnd, x$message)
  }

  tryCatch(
    error = add_err,
    withCallingHandlers(
      message = add_msg,
      warning = add_wrn,
      expr
    )
  )
  return(cnd)
}

#' @rdname conditions
catch_conditions <- function(expr) {
  cnd <- vector(mode = "list")
  add_msg <- function(x) {
    cnd <<- append(cnd, list(x))
    invokeRestart("muffleMessage")
  }
  add_wrn <- function(x) {
    cnd <<- append(cnd, list(x))
    invokeRestart("muffleWarning")
  }
  add_err <- function(x) {
    cnd <<- append(cnd, list(x))
  }

  tryCatch(
    error = add_err,
    withCallingHandlers(
      message = add_msg,
      warning = add_wrn,
      expr
    )
  )
  return(cnd)
}
