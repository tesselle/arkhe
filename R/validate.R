# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# AbundanceMatrix ==============================================================
setValidity(
  Class = "AbundanceMatrix",
  method = function(object) {
    # Get data
    n <- nrow(object)
    samples <- object@samples
    groups <- object@groups
    totals <- object@totals
    from <- object@dates_from
    to <- object@dates_to

    cnd <- list(
      validate(assert_length(samples, n, empty = FALSE)),
      validate(assert_length(groups, n, empty = TRUE)),
      validate(assert_length(from, n, empty = TRUE)),
      validate(assert_length(totals, n, empty = FALSE)),
      validate(assert_length(to, n, empty = TRUE))
    )

    if (!is_empty(totals)) {
      cnd <- c(cnd, validate(assert_numeric(totals, "positive")))
    }

    if (!is_empty(from) & !is_empty(to)) {
      rel <- validate(assert_relation(from, to, "smaller", na.rm = TRUE))
      cnd <- c(cnd, rel)
    }

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# DataMatrix ===================================================================
setValidity(
  Class = "IntegerMatrix",
  method = function(object) {
    cnd <- list(validate(assert_type(object, "integer")))
    check_class(object, cnd)
  }
)
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    cnd <- list(validate(assert_type(object, "numeric")))
    check_class(object, cnd)
  }
)
setValidity(
  Class = "LogicalMatrix",
  method = function(object) {
    cnd <- list(validate(assert_type(object, "logical")))
    check_class(object, cnd)
  }
)

# IntegerMatrix ================================================================
## CountMatrix -----------------------------------------------------------------
setValidity(
  Class = "CountMatrix",
  method = function(object) {
    cnd <- list(
      validate(assert_numeric(object, "positive", strict = FALSE, na.rm = TRUE))
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
      validate(assert_matrix(object, "symmetric")),
      validate(assert_numeric(object, "positive", strict = FALSE, na.rm = TRUE))
    )
    if (!is_empty(total)) {
      cnd <- c(cnd, validate(assert_scalar(total, "integer")))
    }

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# NumericMatrix ================================================================
## CompositionMatrix -----------------------------------------------------------
setValidity(
  Class = "CompositionMatrix",
  method = function(object) {
    cnd <- list(
      validate(assert_numeric(object, "positive", strict = FALSE, na.rm = TRUE))
    )
    check_class(object, cnd)
  }
)

# LogicalMatrix ================================================================
## StratigraphicMatrix ---------------------------------------------------------
setValidity(
  Class = "StratigraphicMatrix",
  method = function(object) {
    cnd <- list()
    if (!is_empty(object)) {
      cnd <- c(cnd, validate(assert_matrix(object, "square")))
      cnd <- c(cnd, validate(assert_dag(object)))
    }

    # Return cnd, if any
    check_class(object, cnd)
  }
)

# Diagnostic ===================================================================
#' Class Diagnostic
#'
#' @param object An object to which error messages are related.
#' @param conditions A [`list`] of condition messages.
#' @return
#'  Throw an error if `conditions` is of non-zero length, invisibly returns
#'  `TRUE` if not.
#' @author N. Frerebeau
#' @keywords internal
#' @export
check_class <- function(object, conditions) {
  cnd <- compact(is_empty, conditions)
  if (has_length(cnd)) {
    err <- sprintf("<%s> instance initialization:\n%s", class(object),
                   paste0("* ", cnd, collapse = "\n"))
    throw_error("arkhe_error_class", err, call = NULL)
  }

  invisible(TRUE)
}
