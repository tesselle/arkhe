# CONDITIONS
#' @include check.R
NULL

#' Conditions
#'
#' \code{throw_error} stops execution of the current expression and executes an
#' error action.
#'
#' \code{catch_conditions} handles unusual conditions.
#' @param .subclass A \code{\link{character}} string specifying the class of
#'  the message to be returned.
#' @param message A \code{\link{character}} string specifying the message to be
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
catch_conditions <- function(expr) {
  cnd <- list()
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
