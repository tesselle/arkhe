# CONDITIONS
#' @include check.R
NULL

#' Conditions
#'
#' * `throw_error()` stops execution of the current expression and executes an
#' error action.
#' * `catch_conditions()` handles unusual conditions.
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

#' Class Diagnostic
#'
#' @param object An object to which error messages are related.
#' @param conditions A [`list`] of condition messages.
#' @return
#'  Throw an error if `conditions` is of non-zero length, invisibly returns
#'  `TRUE` if not.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
check_class <- function(object, conditions) {
  cnd <- compact(is_empty, conditions)
  cnd <- unlist(cnd, recursive = FALSE)

  # Check if any warning
  # wrn_idx <- vapply(X = cnd, FUN = is_warning, FUN.VALUE = logical(1))
  # if (any(wrn_idx)) {
  #   wrn_msg <- vapply(X = cnd[wrn_idx], FUN = `[[`,
  #                     FUN.VALUE = character(1), "message")
  #   wrn <- sprintf("<%s> instance initialization:\n%s", class(object),
  #                  paste0("* ", unlist(wrn_msg), collapse = "\n"))
  #   throw_warning("arkhe_warning_class", wrn, call = NULL)
  # }

  # Check if any error
  err_idx <- vapply(X = cnd, FUN = is_error, FUN.VALUE = logical(1))
  if (any(err_idx)) {
    err_msg <- vapply(X = cnd[err_idx], FUN = `[[`,
                      FUN.VALUE = character(1), "message")
    err <- sprintf("<%s> instance initialization:\n%s", class(object),
                   paste0("* ", err_msg, collapse = "\n"))
    throw_error("arkhe_error_class", err, call = NULL)
  }

  invisible(TRUE)
}
