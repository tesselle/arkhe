# CHRONOLOGY

# Get ==========================================================================
#' @export
#' @rdname chronology
#' @aliases get_dates,GenericMatrix-method
setMethod(
  f = "get_dates",
  signature = "GenericMatrix",
  definition = function(object) {
    d <- data.frame(value = object@date_values, error = object@date_errors)
    if (nrow(d) > 0) rownames(d) <- object@row_names
    d
  }
)

# Set ==========================================================================
#' @export
#' @rdname chronology
#' @aliases set_dates,GenericMatrix,NULL-method
setMethod(
  f = "set_dates<-",
  signature = c("GenericMatrix", "NULL"),
  definition = function(object, value) {
    object@date_values <- numeric(0)
    object@date_errors <- numeric(0)
    object
  }
)

#' @export
#' @rdname chronology
#' @aliases set_dates,GenericMatrix,numeric-method
setMethod(
  f = "set_dates<-",
  signature = c("GenericMatrix", "numeric"),
  definition = function(object, value) {
    if (getOption("arkhe.verbose")) message("Errors are missing, NA generated.")

    # Value is a numeric or integer vector
    x <- c(value, rep(NA_real_, length.out = length(value)))
    m <- matrix(data = x, ncol = 2)
    rownames(m) <- names(value)

    set_dates(object) <- m
    object
  }
)

#' @export
#' @rdname chronology
#' @aliases set_dates,GenericMatrix,character-method
setMethod(
  f = "set_dates<-",
  signature = c("GenericMatrix", "character"),
  definition = function(object, value) {
    if (getOption("arkhe.verbose")) message("Errors are missing, NA generated.")

    # Value is a character vector
    # Try to convert from roman numbers
    roman <- suppressWarnings(utils::as.roman(value))
    if (anyNA(roman)) stop("Incorrect roman number.", call. = FALSE)
    x <- c(as.numeric(roman), rep(NA_real_, length.out = length(roman)))
    m <- matrix(data = x, ncol = 2)
    rownames(m) <- names(value)
    set_dates(object) <- m
    object
  }
)

#' @export
#' @rdname chronology
#' @aliases set_dates,GenericMatrix,list-method
setMethod(
  f = "set_dates<-",
  signature = c("GenericMatrix", "list"),
  definition = function(object, value) {

    # Value is a list
    if (all(c("value", "error") %in% names(value))) {
      x <- value[c("value", "error")]
    } else {
      stop("'value' is a list, ",
           "but does not have components 'value' and 'error'.",
           call. = FALSE)
    }

    m <- as.matrix(cbind.data.frame(x))
    set_dates(object) <- m
    object
  }
)

#' @export
#' @rdname chronology
#' @aliases set_dates,GenericMatrix,data.frame-method
setMethod(
  f = "set_dates<-",
  signature = c("GenericMatrix", "data.frame"),
  definition = function(object, value) {
    # Value is a data.frame
    m <- data.matrix(value)
    set_dates(object) <- m
    object
  }
)

#' @export
#' @rdname chronology
#' @aliases set_dates,GenericMatrix,matrix-method
setMethod(
  f = "set_dates<-",
  signature = c("GenericMatrix", "matrix"),
  definition = function(object, value) {

    # Value is a matrix
    if (ncol(value) >= 2) {
      if (all(c("value", "error") %in% colnames(value))) {
        x <- value[, "value"]
        y <- value[, "error"]
      } else {
        x <- value[, 1]
        y <- value[, 2]
      }
    } else {
      stop("'value' must have at least 2 columns.", call. = FALSE)
    }

    i <- nrow(object)
    j <- nrow(value)

    # If rownames, match by names
    rows_value <- rownames(value)
    rows_object <- rownames(object)
    if (!is.null(rows_value) && !is.null(rows_object)) {
      index <- match(rows_value, rows_object)
      no_match <- detect(f = is.na, x = index)
      if (any(no_match)) {
        warning(
          ngettext(
            sum(no_match),
            "The following date do not match and was skiped:\n",
            "The following dates do not match and were skiped:\n"
          ),
          paste0("* ", rows_value[no_match], collapse = "\n"),
          call. = FALSE
        )
      }
      index_clean <- compact(f = is.na, x = index)
      dates <- matrix(NA_real_, nrow = i, ncol = 2)
      dates[index_clean, ] <- value[!no_match]
      k <- sum(!no_match)
    } else if (j == i || j == 0) {
      # If object and value have the same dimensions or value is unset
      dates <- value
      k <- nrow(dates)
    } else {
      stop("Cannot interpret 'value' in a suitable way.", call. = FALSE)
    }

    object@date_values <- dates[, 1L]
    object@date_errors <- dates[, 2L]
    methods::validObject(object)

    if (getOption("arkhe.verbose")) {
      message(sprintf(ngettext(k, "%d date was set.", "%d dates were set."), k))
    }

    object
  }
)
