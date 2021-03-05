# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "AbundanceMatrix",
  definition = function(object) {
    mtx <- methods::as(object, "matrix")
    mp <- paste0(dim(object), collapse = " x ")
    cat(
      sprintf("<%s: %s>", class(object), mp),
      utils::capture.output(mtx),
      sep = "\n"
    )
    invisible(object)
  }
)
