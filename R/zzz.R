.onLoad <- function(libname, pkgname) {
  op <- options()
  op.arkhe <- list(
    arkhe.autodetect = TRUE,
    arkhe.verbose = TRUE
  )
  toset <- !(names(op.arkhe) %in% names(op))
  if(any(toset)) options(op.arkhe[toset])

  invisible()
}
