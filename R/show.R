# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "DataMatrix",
  definition = function(object) {
    mp <- paste0(dim(object), collapse = " x ")
    cat(
      sprintf("<%s: %s>", class(object), mp),
      utils::capture.output(object@.Data),
      sep = "\n"
    )
    invisible(object)
  }
)
