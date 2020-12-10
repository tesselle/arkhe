# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# DataMatrix ===================================================================
setValidity(
  Class = "DataMatrix",
  method = function(object) {
    # Get data
    values <- object@values
    size <- object@size
    row_names <- object@row_names
    column_names <- object@column_names
    group_names <- object@group_names

    cnd <- list(
      # Check data
      catch_conditions(check_length(values, prod(size))),
      catch_conditions(check_infinite(values)),
      catch_conditions(check_missing(values)),
      # Check dimnames
      # TODO: check unique
      catch_conditions(check_length(row_names, size[[1L]])),
      catch_conditions(check_length(column_names, size[[2L]])),
      # Check groups
      catch_conditions(check_length(group_names, size[[1L]], strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# IntegerMatrix ================================================================
# CountMatrix ------------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    # Get data
    data <- object@values

    cnd <- list(
      # Check data
      catch_conditions(
        check_numbers(data, "positive", strict = FALSE, na.rm = TRUE)
      )
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# NumericMatrix ================================================================
# AbundanceMatrix --------------------------------------------------------------
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    values <- object@values
    totals <- object@totals
    size <- object@size

    cnd <- list(
      # Check values
      catch_conditions(check_numbers(values, "positive", strict = FALSE)),
      catch_conditions(check_missing(values)),
      catch_conditions(check_infinite(values)),
      # Check totals
      # TODO: check constant sum (?)
      catch_conditions(check_numbers(totals, "positive", strict = FALSE)),
      catch_conditions(check_length(totals, size[[1L]])),
      catch_conditions(check_missing(totals)),
      catch_conditions(check_infinite(totals))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)
# OccurrenceMatrix -------------------------------------------------------------
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    # Get data
    values <- as.matrix(object)
    n <- object@n

    cnd <- list(
      # Check values
      catch_conditions(check_symmetric(values)),
      catch_conditions(check_numbers(values, "positive", strict = FALSE)),
      # Check n
      catch_conditions(check_scalar(n, "integer", strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)
# SimilarityMatrix -------------------------------------------------------------
setValidity(
  Class = "SimilarityMatrix",
  method = function(object) {
    # Get data
    values <- as.matrix(object)
    method <- object@method

    cnd <- list(
      # Check values
      catch_conditions(check_symmetric(values)),
      # Check method
      catch_conditions(check_scalar(method, "character", strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# LogicalMatrix ================================================================
# StratigraphicMatrix ----------------------------------------------------------
setValidity(
  Class = "StratigraphicMatrix",
  method = function(object) {
    # Get data
    values <- as.matrix(object)

    if (nrow(values) > 0) {
      cnd <- list(
        # Check values
        catch_conditions(check_square(values)),
        catch_conditions(check_dag(values))
      )
    } else {
      cnd <- list()
    }

    # Return cnd, if any
    check_class(object, cnd)
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
  wrn_idx <- vapply(X = cnd, FUN = is_warning, FUN.VALUE = logical(1))
  if (any(wrn_idx)) {
    wrn_msg <- vapply(X = cnd[wrn_idx], FUN = `[[`,
                      FUN.VALUE = character(1), "message")
    wrn <- sprintf("<%s> instance initialization:\n%s", class(object),
                   paste0("* ", unlist(wrn_msg), collapse = "\n"))
    throw_warning("arkhe_warning_class", wrn, call = NULL)
  }

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
