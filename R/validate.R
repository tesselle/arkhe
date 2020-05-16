# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# ======================================================================= Matrix
setValidity(
  Class = "GenericMatrix",
  method = function(object) {
    # Get data
    id <- object@id
    dates <- object@dates
    coordinates <- object@coordinates
    size <- dim(object)

    # Check
    cnd <- list(
      # Check id
      # catch_conditions(check_uuid(id)),
      # catch_conditions(check_scalar(id, "character"))
    )
    if (length(dates) > 0) {
      cnd <- append(
        cnd,
        list(
          # Check dates
          catch_conditions(check_length(dates, size[[1L]])),
          catch_conditions(check_lengths(dates, 2)),
          catch_conditions(check_names(dates, rownames(object)))
        )
      )
    }
    if (length(coordinates) > 0) {
      cnd <- append(
        cnd,
        list(
          # Check coordinates
          catch_conditions(check_length(coordinates, size[[1L]])),
          catch_conditions(check_lengths(coordinates, 3)),
          catch_conditions(check_names(coordinates, rownames(object)))
        )
      )
    }

    # Return cnd if any
    check_class(object, cnd)
  }
)

# =================================================================== DataMatrix
setValidity(
  Class = "DataMatrix",
  method = function(object) {
    # Get data
    data <- object@values
    length <- length(object)

    cnd <- list(
      # Check data
      catch_conditions(check_length(data, length)),
      catch_conditions(check_infinite(data)),
      catch_conditions(check_missing(data))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# ================================================================ IntegerMatrix
# ------------------------------------------------------------------ CountMatrix
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

# ================================================================ NumericMatrix
# -------------------------------------------------------------- AbundanceMatrix
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    data <- object@values
    totals <- object@totals
    size <- nrow(object)

    cnd <- list(
      # Check data
      catch_conditions(
        check_numbers(data, "positive", strict = FALSE, na.rm = TRUE)
      ),
      # Check totals
      catch_conditions(check_length(totals, size)),
      catch_conditions(check_missing(totals)),
      catch_conditions(check_infinite(totals))
    )
    # TODO: check constant sum.

    # Return cnd, if any
    check_class(object, cnd)
  }
)
# ------------------------------------------------------------- OccurrenceMatrix
setValidity(
  Class = "OccurrenceMatrix",
  method = function(object) {
    # Get data
    data <- as.matrix(object)
    n <- object@n

    cnd <- list(
      # Check data
      catch_conditions(check_symmetric(data)),
      catch_conditions(check_numbers(data, "positive",
                                     strict = FALSE, na.rm = TRUE)),
      # Check n
      catch_conditions(check_scalar(n, "integer"))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)
# ------------------------------------------------------------- SimilarityMatrix
setValidity(
  Class = "SimilarityMatrix",
  method = function(object) {
    # Get data
    data <- as.matrix(object)
    method <- object@method

    cnd <- list(
      # Check data
      catch_conditions(check_symmetric(data)),
      # Check method
      catch_conditions(check_scalar(method, "character")),
      catch_conditions(check_missing(method))
    )

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# ================================================================ LogicalMatrix
# ---------------------------------------------------------- StratigraphicMatrix
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
