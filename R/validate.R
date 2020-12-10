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
      catch_conditions(check_length(row_names, size[[1L]])),
      catch_conditions(check_length(column_names, size[[2L]]))
      # Check extra slots
      # catch_conditions(check_length(group_names, size[[1L]]))
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
      catch_conditions(check_numbers(values, "positive", strict = FALSE,
                                     na.rm = TRUE)),
      catch_conditions(check_missing(values)),
      catch_conditions(check_infinite(values)),
      # Check totals
      catch_conditions(check_length(totals, size[[1L]])),
      catch_conditions(check_missing(totals)),
      catch_conditions(check_infinite(totals))
    )
    # TODO: check constant sum.

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
      catch_conditions(check_numbers(values, "positive", strict = FALSE,
                                     na.rm = TRUE))
    )
    # Check n
    if (length(values) > 0) {
      cnd <- append(cnd, catch_conditions(check_scalar(n, "integer")))
    }

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
      catch_conditions(check_symmetric(values))
    )
    # Check method
    if (length(values) > 0) {
      cnd <- append(
        cnd,
        catch_conditions(check_scalar(method, "character"))
      )
    }

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
    data <- as.matrix(object)

    if (nrow(data) > 0) {
      cnd <- list(
        # Check data
        catch_conditions(check_square(data)),
        catch_conditions(check_dag(data))
      )
    } else {
      cnd <- list()
    }

    # Return cnd, if any
    check_class(object, cnd)
  }
)
