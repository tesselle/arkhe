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
    dates <- object@dates
    tpq <- object@tpq
    taq <- object@taq

    cnd <- list(
      validate(assert_length(samples, n, empty = FALSE)),
      validate(assert_length(groups, n, empty = TRUE)),
      validate(assert_length(dates, n, empty = TRUE)),
      validate(assert_length(tpq, n, empty = TRUE)),
      validate(assert_length(taq, n, empty = TRUE)),
      validate(assert_length(totals, n, empty = FALSE))
    )

    if (!is_empty(totals)) {
      cnd <- c(cnd, validate(assert_numeric(totals, "positive")))
    }

    if (!is_empty(tpq) & !is_empty(taq)) {
      rel <- validate(assert_relation(tpq, taq, "lower", na.rm = TRUE))
      cnd <- c(cnd, rel)
    }

    if (!is_empty(dates) & !is_empty(tpq)) {
      rel <- validate(assert_relation(dates, tpq, "greater", na.rm = TRUE))
      cnd <- c(cnd, rel)
    }

    if (!is_empty(dates) & !is_empty(taq)) {
      rel <- validate(assert_relation(dates, taq, "lower", na.rm = TRUE))
      cnd <- c(cnd, rel)
    }

    # Return cnd, if any
    check_class(object, cnd)
  }
)

setValidity(
  Class = "AbundanceSummary",
  method = function(object) {
    # Get data
    groups <- object@groups
    rows <- object@rows
    cols <- object@columns
    replicates <- object@replicates
    chronology <- object@chronology
    n <- length(groups)

    cnd <- list(
      validate(assert_length(rows, n, empty = FALSE)),
      validate(assert_length(cols, n, empty = FALSE)),
      validate(assert_length(replicates, n, empty = FALSE)),
      validate(assert_dimensions(chronology, c(n, 2)))
    )

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
