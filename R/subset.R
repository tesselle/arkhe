# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,DataMatrix-method
setMethod(
  f = "[",
  signature = c(x = "DataMatrix"),
  function(x, i, j, ..., drop = TRUE) {
    z <- methods::callNextMethod()
    if (is.null(dim(z))) {
      return(z)
    }

    if (!missing(i)) {
      slots <- methods::slotNames(x)
      for (s in slots) {
        old <- methods::slot(x, s)
        if (length(old) == nrow(x)) {
          methods::slot(x, s, check = FALSE) <- old[i]
        }
      }
    }

    methods::initialize(x, z)
  }
)

# Replace ======================================================================
## [<- -------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [<-,DataMatrix-method
setMethod(
  f = "[<-",
  signature = c(x = "DataMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)

## [[<- ------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[<-,DataMatrix-method
setMethod(
  f = "[[<-",
  signature = c(x = "DataMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)
