# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,AbundanceMatrix-method
setMethod(
  f = "[",
  signature = c(x = "AbundanceMatrix"),
  function(x, i, j, ..., drop = TRUE) {
    z <- methods::callNextMethod()

    if (is.null(dim(z))) {
      return(z)
    }

    if (!missing(i)) {
      samples <- x@samples
      groups <- x@groups
      if (length(samples) > 0) samples <- samples[i]
      if (length(groups) > 0) groups <- groups[i]
      methods::initialize(x, z, samples = samples, groups = groups)
    } else{
      methods::initialize(x, z)
    }
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix"),
  function(x, i, j, ..., drop = TRUE) {
    totals <- x@total
    x@total <- numeric(0)

    z <- methods::callNextMethod()

    if (is.null(dim(z))) {
      return(z)
    }

    if (!missing(i) & length(totals) > 0) {
      methods::initialize(x, z, total = totals[i])
    } else{
      methods::initialize(x, z)
    }
  }
)

# Replace ======================================================================
## [<- -------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [<-,AbundanceMatrix-method
setMethod(
  f = "[<-",
  signature = c(x = "AbundanceMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)

## [[<- ------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[<-,AbundanceMatrix-method
setMethod(
  f = "[[<-",
  signature = c(x = "AbundanceMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)
