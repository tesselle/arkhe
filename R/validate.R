# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# Matrix =======================================================================
setValidity(
  Class = "Matrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    id <- object@id
    dates <- object@dates
    coordinates <- object@coordinates

    # Check
    errors <- list(
      # Check data
      catch_conditions(check_missing(data)),
      catch_conditions(check_infinite(data)),
      # Check id
      catch_conditions(check_uuid(id))
    )
    if (nrow(data) > 0 && length(dates) != 0) {
      errors <- append(
        errors,
        list(
          # Check dates
          catch_conditions(check_length(dates, nrow(data))),
          catch_conditions(check_lengths(dates, 2)),
          catch_conditions(check_names(dates, margin = 1, rownames(data)))
        )
      )
    }
    if (nrow(data) > 0 && length(coordinates) != 0) {
      errors <- append(
        errors,
        list(
          # Check coordinates
          catch_conditions(check_length(coordinates, nrow(data))),
          catch_conditions(check_lengths(coordinates, 3)),
          catch_conditions(check_names(coordinates, margin = 1, rownames(data)))
        )
      )
    }

    # Return errors if any
    check_class(object, errors)
  }
)

# NumericMatrix ================================================================
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    if (nrow(data) > 0) {
      errors <- list(
        # Check data
        catch_conditions(check_type(data, "numeric"))
      )
    } else {
      errors <- list()
    }

    # Return errors if any
    check_class(object, errors)
  }
)

## CountMatrix -----------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    # dates <- object@dates
    # coordinates <- object@coordinates

    errors <- list(
      # Check data
      catch_conditions(check_numbers(data, "positive", strict = FALSE)),
      catch_conditions(check_numbers(data, "whole"))
    )
    # Messages
    # TODO: warning instead of message?
    # if (all(is_binary(data)))
    #   message("Your matrix contains only 0s and 1s.\n",
    #           "You should consider using an incidence matrix instead.")

    # Return errors, if any
    check_class(object, errors)
  }
)

## AbundanceMatrix -------------------------------------------------------------
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    totals <- object@totals
    # dates <- object@dates
    # coordinates <- object@coordinates

    errors <- list(
      # Check data
      catch_conditions(check_numbers(data, "positive", strict = FALSE)),
      # Check totals
      catch_conditions(check_length(totals, nrow(data)))
    )
    if (nrow(data) > 0) {
      errors <- append(
        errors,
        list(
          # Check totals
          catch_conditions(check_constant(rowSums(data)))
        )
      )
    }
    # print(totals)
    # Return errors, if any
    check_class(object, errors)
  }
)

## OccurrenceMatrix ------------------------------------------------------------
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      catch_conditions(check_symmetric(data))
    )

    # Return errors, if any
    check_class(object, errors)
  }
)

## SimilarityMatrix ------------------------------------------------------------
setValidity(
  Class = "SimilarityMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    method <- object@method

    errors <- list(
      # Check data
      catch_conditions(check_symmetric(data)),
      # Check method
      catch_conditions(check_scalar(method, "character")),
      catch_conditions(check_missing(method))
    )

    # Return errors, if any
    check_class(object, errors)
  }
)

# LogicalMatrix ================================================================
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    if (nrow(data) > 0) {
      errors <- list(
        # Check data
        catch_conditions(check_type(data, "logical"))
      )
    } else {
      errors <- list()
    }

    # Return errors if any
    check_class(object, errors)
  }
)

# -------------------------------------------------------------- IncidenceMatrix
# setValidity(
#   Class = "IncidenceMatrix",
#   method = function(object) {
#
#   }
# )

# ---------------------------------------------------------- StratigraphicMatrix
setValidity(
  Class = "StratigraphicMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    units <- object@units

    if (nrow(data) > 0) {
      errors <- list(
        # Check data
        catch_conditions(check_square(data)),
        catch_conditions(check_dag(data)),
        # Check unit names
        catch_conditions(check_length(units, nrow(data)))
      )
    } else {
      errors <- list()
    }

    # Return errors, if any
    check_class(object, errors)
  }
)

# =================================================================== Diagnostic
#' Class Diagnostic
#'
#' @param object An object to which error messages are related.
#' @param conditions A \code{\link{list}} of condition messages.
#' @param verbose A \code{\link{logical}} scalar: should extra information
#'  be reported?
#' @return
#'  Throw an error if \code{conditions} is of non-zero length, returns \code{TRUE}
#'  if not.
#' @author N. Frerebeau
#' @keywords internal error
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
