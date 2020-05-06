# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# ======================================================================= Matrix
setValidity(
  Class = "GenericMatrix",
  method = function(object) {
    # Get data
    id <- object@id
    size <- object@size
    row_names <- object@row_names
    column_names <- object@column_names
    dates <- object@dates
    coordinates <- object@coordinates

    # Check
    errors <- list(
      # Check id
      catch_conditions(check_uuid(id)),
      catch_conditions(check_scalar(id, "character")),
      # Check size
      catch_conditions(check_length(size, 2)),
      # Check row names
      catch_conditions(check_length(row_names, size[[1L]])),
      # Check column names
      catch_conditions(check_length(column_names, size[[2L]]))
    )
    if (length(dates) > 0) {
      errors <- append(
        errors,
        list(
          # Check dates
          catch_conditions(check_length(dates, size[[1L]])),
          catch_conditions(check_lengths(dates, 2)),
          catch_conditions(check_names(dates, row_names))
        )
      )
    }
    if (length(coordinates) > 0) {
      errors <- append(
        errors,
        list(
          # Check coordinates
          catch_conditions(check_length(coordinates, size[[1L]])),
          catch_conditions(check_lengths(coordinates, 3)),
          catch_conditions(check_names(coordinates, row_names))
        )
      )
    }

    # Return errors if any
    check_class(object, errors)
  }
)

# =================================================================== DataMatrix
setValidity(
  Class = "DataMatrix",
  method = function(object) {
    # Get data
    data <- object@data
    size <- object@size

    errors <- list(
      # Check data
      catch_conditions(check_length(data, prod(size))),
      catch_conditions(check_missing(data)),
      catch_conditions(check_infinite(data))
    )

    # Return errors, if any
    check_class(object, errors)
  }
)

# ================================================================ IntegerMatrix
# ------------------------------------------------------------------ CountMatrix
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    # Get data
    data <- object@data

    errors <- list(
      # Check data
      catch_conditions(check_numbers(data, "positive",
                                     strict = FALSE, na.rm = TRUE))
    )
    # TODO: check if only 0s and 1s (?)

    # Return errors, if any
    check_class(object, errors)
  }
)

# ================================================================ NumericMatrix
# -------------------------------------------------------------- AbundanceMatrix
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    data <- object@data
    size <- object@size
    totals <- object@totals

    errors <- list(
      # Check data
      catch_conditions(check_numbers(data, "positive",
                                     strict = FALSE, na.rm = TRUE)),
      # Check totals
      catch_conditions(check_length(totals, size[[1L]])),
      catch_conditions(check_missing(totals)),
      catch_conditions(check_infinite(totals))
    )
    # TODO: check constant sum.

    # Return errors, if any
    check_class(object, errors)
  }
)
# ------------------------------------------------------------- OccurrenceMatrix
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    # Get data
    data <- as_matrix(object)
    n <- object@n

    errors <- list(
      # Check data
      catch_conditions(check_symmetric(data)),
      catch_conditions(check_numbers(data, "positive",
                                     strict = FALSE, na.rm = TRUE)),
      # Check n
      catch_conditions(check_scalar(n, "integer"))
    )

    # Return errors, if any
    check_class(object, errors)
  }
)
# ------------------------------------------------------------- SimilarityMatrix
setValidity(
  Class = "SimilarityMatrix",
  method = function(object) {
    # Get data
    data <- as_matrix(object)
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

# ================================================================ LogicalMatrix
# ---------------------------------------------------------- StratigraphicMatrix
setValidity(
  Class = "StratigraphicMatrix",
  method = function(object) {
    # Get data
    data <- as_matrix(object)

    if (nrow(data) > 0) {
      errors <- list(
        # Check data
        catch_conditions(check_square(data)),
        catch_conditions(check_dag(data))
      )
    } else {
      errors <- list()
    }

    # Return errors, if any
    check_class(object, errors)
  }
)
