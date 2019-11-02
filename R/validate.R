# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# SpaceTime ====================================================================
setValidity(
  Class = "SpaceTime",
  method = function(object) {
    # Get data
    dates <- object@dates
    coordinates <- object@coordinates
    epsg <- object@epsg

    # Check dates
    errors <- list(
      dates = c(
        catch_conditions(check_type(dates, expected = "numeric")),
        catch_conditions(check_infinite(dates)),
        catch_conditions(check_names(dates, expected = c("value", "error"),
                                     margin = 2))
      ),
      coordinates = c(
        catch_conditions(check_type(coordinates, expected = "numeric")),
        catch_conditions(check_infinite(coordinates)),
        catch_conditions(check_names(coordinates, expected = c("X", "Y", "Z"),
                                     margin = 2))
      ),
      epsg = c(
        catch_conditions(check_scalar(epsg, expected = "integer")),
        catch_conditions(check_missing(epsg))
      )
    )

    # Return errors if any
    throw_error_class(object, errors)
  }
)
# Matrix =======================================================================
setValidity(
  Class = "Matrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    id <- object@id

    errors <- list(
      # Check data
      data = c(
        catch_conditions(check_missing(data)),
        catch_conditions(check_infinite(data))
      ),
      # Check id
      id = c(
        catch_conditions(check_uuid(id))
      )
    )

    # Return errors if any
    throw_error_class(object, errors)
  }
)
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")
    dates <- object@dates
    coordinates <- object@coordinates
    n <- nrow(data)

    errors <- list()
    if (length(dates) != 0 && nrow(dates) > 0) {
      # Check dates
      errors[["dates"]] <- c(
        catch_conditions(check_length(dates, expected = n * 2))
      )
    }
    if (length(coordinates) != 0 && nrow(coordinates) > 0) {
      # Check coordinates
      errors[["coordinates"]] <- c(
        catch_conditions(check_length(coordinates, expected = n * 3))
      )
    }

    # Return errors if any
    throw_error_class(object, errors)
  }
)

# NumericMatrix ================================================================
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      data = catch_conditions(check_type(data, expected = "numeric"))
    )

    # Return errors if any
    throw_error_class(object, errors)
  }
)

## CountMatrix -----------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      data = c(
        catch_conditions(check_numbers(data, expected = "positive",
                                       strict = FALSE)),
        catch_conditions(check_numbers(data, expected = "whole"))
      )
    )
    # Messages
    # TODO: warning instead of message?
    if (all(is_binary(data)))
      message("Your matrix contains only 0s and 1s.\n",
              "You should consider using an incidence matrix instead.")

    # Return errors, if any
    throw_error_class(object, errors)
  }
)

## FrequencyMatrix -------------------------------------------------------------
setValidity(
  Class = "FrequencyMatrix",
  method = function(object) {
    # Get data
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    totals <- object@totals
    dates <- object@dates
    coordinates <- object@coordinates
    n <- nrow(data)

    errors <- list(
      # Check data
      data = c(
        catch_conditions(check_numbers(data, expected = "positive",
                                       strict = FALSE)),
        catch_conditions(check_constant(data))
      ),
      # Check totals
      totals = c(
        catch_conditions(check_length(totals, expected = n))
      )
    )

    # Return errors, if any
    throw_error_class(object, errors)
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
      data = c(
        catch_conditions(check_symmetric(data))
      )
    )

    # Return errors, if any
    throw_error_class(object, errors)
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
      data = c(
        catch_conditions(check_symmetric(data))
      ),
      # Check method
      method = c(
        catch_conditions(check_scalar(method, expected = "character")),
        catch_conditions(check_missing(method))
      )
    )

    # Return errors, if any
    throw_error_class(object, errors)
  }
)

# LogicalMatrix ================================================================
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    # Get data
    data <- S3Part(object, strictS3 = TRUE, "matrix")

    errors <- list(
      # Check data
      data = catch_conditions(check_type(data, expected = "logical"))
    )

    # Return errors if any
    throw_error_class(object, errors)
  }
)

## IncidenceMatrix -------------------------------------------------------------
# setValidity(
#   Class = "IncidenceMatrix",
#   method = function(object) {
#
#   }
# )
