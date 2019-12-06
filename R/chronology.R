# CHRONOLOGY

# ========================================================================== Get
#' @export
#' @rdname chronology
#' @aliases get_dates,Matrix-method
setMethod(
  f = "get_dates",
  signature = "Matrix",
  definition = function(object) {
    if (length(object@dates) != 0) {
      dates <- do.call(rbind, object@dates)
      colnames(dates) <- c("value", "error")
      as.data.frame(dates, stringsAsFactors = FALSE)
    } else {
      data.frame(value = numeric(0), error = numeric(0))
    }
  }
)

# ========================================================================== Set
#' @param value A \code{\link{matrix}}, \code{\link{data.frame}},
#'  \code{\link{list}} or a \code{\link{numeric}}, \code{\link{integer}} or
#'  \code{\link{character}} vector.
#' @details
#'  Try to interpret \code{value} in a suitable way.
#' @return A \code{\link{numeric}} matrix.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
make_dates <- function(value) {
  if (is.matrix(value) || is.data.frame(value)) {
    # Value is a matrix or data.frame
    value <- data.matrix(value)
    if (ncol(value) >= 2) {
      if (all(c("value", "error") %in% colnames(value))) {
        x <- value[, "value"]
        y <- value[, "error"]
      } else {
        x <- value[, 1]
        y <- value[, 2]
      }
    } else {
      stop("`value` must have at least 2 columns.", call. = FALSE)
    }
  } else if (is.list(value)) {
    # Value is a list
    if (all(c("value", "error") %in% names(value))) {
      x <- value[["value"]]
      y <- value[["error"]]
    } else {
      stop("`value` is a list, ",
           "but does not have components 'value' and 'error'.",
           call. = FALSE)
    }
  } else if (is.numeric(value) || is.integer(value)) {
    # Value is a numeric or integer vector
    x <- value
    y <- rep_len(NA, length.out = length(x))
    if (getOption("verbose")) message("Errors are missing, NA generated.")
  } else if (is.character(value)) {
    # Value is a character vector
    # Try to convert from roman numbers
    roman <- suppressWarnings(utils::as.roman(value))
    if (anyNA(roman)) {
      stop("Incorrect roman number.", call. = FALSE)
    } else {
      x <- as.numeric(roman)
      y <- rep_len(NA, length.out = length(x))
    }
  } else if (is.null(value)) {
    # Value is NULL
    x <- y <- numeric(0)
  } else {
    stop("A numeric, integer or character vector, ",
         "a list, a matrix or a data frame is expected.", call. = FALSE)
  }

  cbind(value = x, error = y)
}

#' @export
#' @rdname chronology
#' @aliases set_dates,Matrix-method
setMethod(
  f = "set_dates<-",
  signature = "Matrix",
  definition = function(object, value) {
    value <- make_dates(value)
    rows_value <- rownames(value)
    rows_object <- rownames(object)

    i <- nrow(object)
    j <- nrow(value)

    if (j == i || j == 0) {
      # If object and value have the same dimensions or value is unset
      dates <- value
      k <- nrow(dates)
    } else if (!is.null(rows_value) && !is.null(rows_object)) {
      # Match by names
      index <- match(rows_value, rows_object)
      no_match <- detect(f = is.na, x = index)
      if (any(no_match)) {
        warning(ngettext(
          sum(no_match),
          "The following date do not match and was skiped:\n",
          "The following dates do not match and were skiped:\n"),
          paste0("* ", rows_value[no_match], collapse = "\n"),
          call. = FALSE)
      }
      index_clean <- compact(f = is.na, x = index)
      dates <- matrix(NA_real_, nrow = i, ncol = 2,
                      dimnames = list(rows_object, c("value", "error")))
      dates[index_clean, ] <- value[!no_match]
      k <- sum(!no_match)
    } else {
      stop("Cannot interpret `value` in a suitable way.", call. = FALSE)
    }

    if (nrow(value) != 0) {
      # Keep original ordering
      f <- factor(rows_object, levels = unique(rows_object))
      dates <- split(dates, f = f, drop = FALSE)
    } else {
      dates <- list()
    }
    object@dates <- dates
    methods::validObject(object)

    if (getOption("verbose")) {
      message(sprintf(ngettext(k, "%d date was set.", "%d dates were set."), k))
    }
    object
  }
)
