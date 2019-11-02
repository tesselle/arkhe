# CONDITIONS
#' @include check.R
NULL

#' Conditions
#'
#' @param message A \code{\link{character}} string specifying the error
#'  message.
#' @param call The call.
#' @param object An object to which error messages are related.
#' @param errors A \code{\link{character}} vector giving the error messages.
#' @param ... Extra arguments.
#' @return
#'  Throw an error if \code{errors} is of non-zero length, returns \code{TRUE}
#'  if not.
#' @author N. Frerebeau
#' @name conditions
#' @keywords internal error
#' @noRd

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

catch_conditions <- function(expr) {
  conditions <- list()
  add_mess <- function(cnd) {
    conditions <<- append(conditions, list(cnd))
    invokeRestart("muffleMessage")
  }
  add_warn <- function(cnd) {
    conditions <<- append(conditions, list(cnd))
    invokeRestart("muffleWarning")
  }
  add_err <- function(cnd) {
    conditions <<- append(conditions, list(cnd))
  }

  tryCatch(
    error = add_err,
    withCallingHandlers(
      message = add_mess,
      warning = add_warn,
      expr
    )
  )
  return(conditions)
}

throw_error_class <- function(object, errors) {
  errors <- compact(is_empty, errors)
  if (!is_empty(errors)) {
    messages <- lapply(
      X = names(errors),
      FUN = function(slot, errors) {
        vapply(X = errors[[slot]], FUN = `[[`, FUN.VALUE = "character", 1)
      },
      errors = errors
    )
    error_msg <- sprintf("%s object initialization:\n*  %s",
                         dQuote(class(object)),
                         paste0(unlist(messages), collapse = "\n*  "))
    err <- structure(
      list(message = error_msg, call = NULL),
      class = c("error_class_initialize", "error", "condition")
    )
    stop(err)
  } else {
    TRUE
  }
}

throw_message_class <- function(class, verbose = getOption("verbose")) {
  msg <- structure(
    list(
      message = sprintf("%s instance initialization...\n", dQuote(class))
    ),
    class = c("message_class_initialize", "message", "condition")
  )
  if (verbose) message(msg)
}
