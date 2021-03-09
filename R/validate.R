# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# AbundanceMatrix ==============================================================
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    samples <- object@samples
    groups <- object@groups

    cnd <- list(
      catch_conditions(check_length(samples, nrow(object), strict = FALSE)),
      catch_conditions(check_length(groups, nrow(object), strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# DataMatrix ===================================================================
setValidity(
  Class = "IntegerMatrix",
  method = function(object) {
    cnd <- list(catch_conditions(check_type(object, "integer")))
    check_class(object, cnd)
  }
)
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    cnd <- list(catch_conditions(check_type(object, "numeric")))
    check_class(object, cnd)
  }
)
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    cnd <- list(catch_conditions(check_type(object, "logical")))
    check_class(object, cnd)
  }
)

# IntegerMatrix ================================================================
## CountMatrix -----------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    cnd <- list(
      catch_conditions(
        check_numbers(object, "positive", strict = FALSE, na.rm = TRUE)
      )
    )
    check_class(object, cnd)
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
      catch_conditions(
        check_numbers(object, "positive", strict = FALSE, na.rm = TRUE)
      ),
      catch_conditions(check_scalar(total, "integer", strict = FALSE))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# NumericMatrix ================================================================
## CompositionMatrix -----------------------------------------------------------
setValidity(
  Class = "CompositionMatrix",
  method = function(object) {
    # Get data
    total <- object@total

    cnd <- list(
      catch_conditions(
        check_numbers(object, "positive", strict = FALSE, na.rm = TRUE)
      ),
      catch_conditions(check_numbers(total, "positive", strict = FALSE)),
      catch_conditions(check_length(total, nrow(object), strict = FALSE))
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

    # Return cnd, if any
    check_class(object, cnd)
  }
)
