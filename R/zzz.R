.onLoad <- function(libname, pkgname) {
  op <- options()
  op.arkhe <- list(
    arkhe.verbose = interactive()
  )
  toset <- !(names(op.arkhe) %in% names(op))
  if(any(toset)) options(op.arkhe[toset])

  invisible()
}
