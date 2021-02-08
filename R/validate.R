# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# DataMatrix ===================================================================
setValidity(
  Class = "DataMatrix",
  method = function(object) {
    # Get data
    groups <- object@groups

    cnd <- list(
      # Check groups
      catch_conditions(check_length(groups, nrow(object), strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

setValidity(
  Class = "IntegerMatrix",
  method = function(object) {
    cnd <- list(catch_conditions(check_type(object, "integer")))
    check_class(object, cnd) # Return cnd, if any
  }
)
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    cnd <- list(catch_conditions(check_type(object, "numeric")))
    check_class(object, cnd) # Return cnd, if any
  }
)
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    cnd <- list(catch_conditions(check_type(object, "logical")))
    check_class(object, cnd) # Return cnd, if any
  }
)

# IntegerMatrix ================================================================
## CountMatrix -----------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    cnd <- list(
      catch_conditions(check_numbers(object, "positive",
                                     strict = FALSE, na.rm = TRUE))
    )
    check_class(object, cnd) # Return cnd, if any
  }
)
## OccurrenceMatrix ------------------------------------------------------------
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    # Get data
    total <- object@total

    cnd <- list(
      catch_conditions(check_symmetric(object)),
      catch_conditions(check_numbers(object, "positive",
                                     strict = FALSE, na.rm = TRUE)),
      catch_conditions(check_scalar(total, "integer", strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# NumericMatrix ================================================================
## AbundanceMatrix -------------------------------------------------------------
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    totals <- object@totals

    cnd <- list(
      catch_conditions(check_numbers(object, "positive",
                                     strict = FALSE, na.rm = TRUE)),
      catch_conditions(check_numbers(totals, "positive", strict = FALSE)),
      catch_conditions(check_length(totals, nrow(object), strict = TRUE)),
      catch_conditions(check_missing(totals)),
      catch_conditions(check_infinite(totals))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# LogicalMatrix ================================================================
## StratigraphicMatrix ---------------------------------------------------------
setValidity(
  Class = "StratigraphicMatrix",
  method = function(object) {
    if (nrow(object) > 0) {
      cnd <- list(
        # Check data
        catch_conditions(check_square(object)),
        catch_conditions(check_dag(object))
      )
    } else {
      cnd <- list()
    }
    check_class(object, cnd) # Return cnd, if any
  }
)

# Diagnostic ===================================================================
#' Class Diagnostic
#'
#' @param object An object to which error messages are related.
#' @param conditions A \code{\link{list}} of condition messages.
#' @return
#'  Throw an error if \code{conditions} is of non-zero length,
#'  returns \code{TRUE} if not.
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

  return(TRUE)
}
