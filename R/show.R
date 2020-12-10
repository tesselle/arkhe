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

setMethod(
  f = "show",
  signature = "MatrixSummary",
  definition = function(object) {

    for (x in object) {
      group <- sprintf("%s (%d observations)", x$group, x$dim[[1L]])
      ndashes <- getOption("width") - nchar(group) - 5
      dashes <- paste0(rep("-", times = ndashes), collapse = "")
      cat(sprintf("--- %s %s\n", group, dashes))
      print(x$stats)
    }

  }
)
