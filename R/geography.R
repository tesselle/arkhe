# GEOGRAPHY

# ========================================================================== Get
#' @export
#' @rdname space
#' @aliases get_coordinates,AbundanceMatrix-method
setMethod(
  f = "get_coordinates",
  signature = "AbundanceMatrix",
  definition = function(object) {
    coords <- object@coordinates
    coords <- as.list(as.data.frame(coords))
    attr(coords, "epsg") <- object@epsg
    coords
  }
)

#' @export
#' @rdname space
#' @aliases get_epsg,AbundanceMatrix-method
setMethod("get_epsg", "AbundanceMatrix", function(object) object@epsg)

# ========================================================================== Set
make_coordinates <- function(value) {
  if (is.matrix(value) | is.data.frame(value)) {
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
      stop("`value` should have at least 2 columns.", call. = FALSE)
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
  } else {
    stop("A list, a matrix or a data frame is expected.",
         call. = FALSE)
  }
  cbind(X = X, Y = Y, Z = Z)
}

#' @export
#' @rdname space
#' @aliases set_coordinates,AbundanceMatrix-method
setMethod(
  f = "set_coordinates<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    object@coordinates <- make_coordinates(value)
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname space
#' @aliases set_epsg,AbundanceMatrix-method
setMethod(
  f = "set_epsg<-",
  signature = "AbundanceMatrix",
  definition = function(object, value) {
    if (!(is.numeric(value) | is.integer(value)) || length(value) != 1)
      stop("`value` should be a length-one numeric or integer vector.",
           call. = FALSE)
    object@epsg <- as.integer(value[1L])
    methods::validObject(object)
    object
  }
)
