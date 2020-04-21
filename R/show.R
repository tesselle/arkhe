# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "DataMatrix",
  definition = function(object) {
    cat(sprintf("<%s: %s>\n", class(object),
                paste0(object@size, collapse = " x ")))
  }
)
setMethod(
  f = "print",
  signature = "DataMatrix",
  definition = function(x) {
    as_matrix(x)
  }
)
