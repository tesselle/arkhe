# GEOGRAPHY

# ========================================================================== Get
#' @export
#' @rdname geography
#' @aliases get_coordinates,Matrix-method
setMethod(
  f = "get_coordinates",
  signature = "Matrix",
  definition = function(object) {
    if (length(object@coordinates) != 0) {
      coords <- do.call(rbind, object@coordinates)
      colnames(coords) <- c("X", "Y", "Z")
      as.data.frame(coords, stringsAsFactors = FALSE)
    } else {
      data.frame(X = numeric(0), Y = numeric(0), Z = numeric(0))
    }
  }
)

# ========================================================================== Set
make_coordinates <- function(value) {
  if (is.matrix(value) || is.data.frame(value)) {
    # Value is a matrix or data.frame
    value <- data.matrix(value)
    colnames(value) <- toupper(colnames(value))
    if (ncol(value) >= 2) {
      if (all(c("X", "Y") %in% colnames(value))) {
        X <- value[, "X"]
        Y <- value[, "Y"]
      } else {
        X <- value[, 1]
        Y <- value[, 2]
      }
    } else {
      stop("`value` must have at least 2 columns.", call. = FALSE)
    }
    if (ncol(value) >= 3) {
      if ("Z" %in% colnames(value)) {
        Z <- value[, "Z"]
      } else {
        Z <- value[, 3]
      }
    } else {
      Z <- rep_len(x = NA_real_, length.out = nrow(value))
      if (getOption("verbose")) message("'Z' is missing, NA generated.")
    }
  } else if (is.list(value)) {
    names(value) <- toupper(names(value))
    if (all(c("X", "Y") %in% names(value))) {
      X <- value[["X"]]
      Y <- value[["Y"]]
    } else {
      stop("`value` is a list, but does not have components 'X' and 'Y'.",
           call. = FALSE)
    }
    if ("Z" %in% names(value)) {
      Z <- value[["Z"]]
    } else {
      Z <- rep_len(x = NA_real_, length.out = length(X))
      if (getOption("verbose")) message("'Z' is missing, NA generated.")
    }
  } else if (is.null(value)) {
    # Value is NULL
    X <- Y <- Z <- numeric(0)
  } else {
    stop("A list, a matrix or a data frame is expected.",
         call. = FALSE)
  }
  cbind(X = X, Y = Y, Z = Z)
}

#' @export
#' @rdname geography
#' @aliases set_coordinates,Matrix-method
setMethod(
  f = "set_coordinates<-",
  signature = "Matrix",
  definition = function(object, value) {
    value <- make_coordinates(value)
    rows_value <- rownames(value)
    rows_object <- rownames(object)

    i <- nrow(object)
    j <- nrow(value)

    if (j == i || j == 0) {
      # If object and value have the same dimensions or value is unset
      coords <- value
      k <- nrow(coords)
    } else if (!is.null(rows_value) && !is.null(rows_object)) {
      # Match by names
      index <- match(rows_value, rows_object)
      no_match <- detect(f = is.na, x = index)
      if (any(no_match)) {
        warning(ngettext(
          sum(no_match),
          "The following coordinates do not match and was skiped:\n",
          "The following coordinates do not match and were skiped:\n"),
          paste0("* ", rows_value[no_match], collapse = "\n"),
          call. = FALSE)
      }
      index_clean <- compact(f = is.na, x = index)
      coords <- matrix(NA_real_, nrow = i, ncol = 3,
                      dimnames = list(rows_object, c("X", "Y", "Z")))
      coords[index_clean, ] <- value[!no_match]
      k <- sum(!no_match)
    } else {
      stop("Cannot interpret `value` in a suitable way.", call. = FALSE)
    }

    if (nrow(value) != 0) {
      # Keep original ordering
      f <- factor(rows_object, levels = unique(rows_object))
      coords <- split(coords, f = f, drop = FALSE)
    } else {
      coords <- list()
    }
    object@coordinates <- coords
    methods::validObject(object)

    if (getOption("verbose")) {
      message(sprintf("%d coordinates were set.", k))
    }
    object
  }
)
