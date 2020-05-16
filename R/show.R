# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "DataMatrix",
  definition = function(object) {
    cat(sprintf("<%s: %s>\n", class(object),
                paste0(dim(object), collapse = " x ")))
    print(as.matrix(object))
  }
)
