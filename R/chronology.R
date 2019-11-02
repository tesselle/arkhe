# CHRONOLOGY

# ========================================================================== Get
#' @export
#' @rdname time
#' @aliases get_dates,AbundanceMatrix-method
setMethod(
  f = "get_dates",
  signature = "AbundanceMatrix",
  definition = function(object) {
    dates <- object@dates
    as.list(as.data.frame(dates))
  }
)

# ========================================================================== Set
make_dates <- function(value) {
  if (is.matrix(value) | is.data.frame(value)) {
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
      stop("`value` should have at least 2 columns.", call. = FALSE)
    }
  } else if(is.list(value)) {
    if (all(c("value", "error") %in% names(value))) {
      x <- value[["value"]]
      y <- value[["error"]]
    } else {
      stop("`value` is a list, ",
           "but does not have components 'value' and 'error'.",
           call. = FALSE)
    }
  } else if (is.numeric(value) || is.integer(value) || is.character(value)) {
    x <- value
    y <- rep_len(0, length.out = length(x))
    if (getOption("verbose")) message("Errors are missing, NA generated.")
  } else if (is.null(value)) {
    x <- y <- numeric(0)
  } else {
    stop("A numeric, integer or character vector, ",
         "a list, a matrix or a data frame is expected.", call. = FALSE)
  }
  # If `x` is a character vector, try to convert from roman numbers
  if (is.character(x)) {
    x <- as.numeric(utils::as.roman(x))
  }
  # Replace NA with zeros
  y[is.na(y)] <- 0
  cbind(value = x, error = y)
}

#' @export
#' @rdname time
#' @aliases set_dates,AbundanceMatrix-method
setMethod(
  f = "set_dates<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    value <- make_dates(value)
    rows_value <- rownames(value)
    rows_object <- rownames(object)
    i <- nrow(object)
    if (all(dim(value) == c(i, 2)) || nrow(value) == 0) {
      # Same dimensions or unset
      B <- value
      k <- nrow(B)
    } else if (!is.null(rows_value) && !is.null(rows_object)) {
      # Match by names
      index <- match(rows_value, rows_object)
      no_match <- detect(f = is.na, x = index)
      if (any(no_match)) {
        warning(ngettext(sum(no_match),
                         "The following date do not match and was skiped:\n",
                         "The following dates do not match and were skiped:\n"),
                paste0("* ", rows_value[no_match], collapse = "\n"),
                call. = FALSE)
      }
      index_clean <- compact(f = is.na, x = index)
      B <- matrix(NA_real_, nrow = i, ncol = 2,
                  dimnames = list(rows_object, c("value", "error")))
      B[index_clean, ] <- value[!no_match]
      k <- sum(!no_match)
    } else {
      stop("Cannot interpret `value` in a suitable way.", call. = FALSE)
    }

    if (nrow(value) != 0) rownames(B) <- rows_object
    object@dates <- B
    methods::validObject(object)
    if (getOption("verbose")) {
      message(sprintf(ngettext(k, "%d date was set.", "%d dates were set."), k))
    }
    object
  }
)
