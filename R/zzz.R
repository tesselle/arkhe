# Set options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.arkhe <- list(
    arkhe.verbose = FALSE
  )
  toset <- !(names(op.arkhe) %in% names(op))
  if(any(toset)) options(op.arkhe[toset])

  invisible()
}
