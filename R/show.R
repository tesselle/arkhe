# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "DataMatrix",
  definition = function(object) {
    mp <- paste0(dim(object), collapse = " x ")
    cat(sprintf("<%s: %s>\n", class(object), mp))
    print(as.matrix(object))
  }
)
