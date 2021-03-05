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
      sites <- x@sites
      groups <- x@groups
      if (length(sites) > 0) sites <- sites[i]
      if (length(groups) > 0) groups <- groups[i]
      methods::initialize(x, z, sites = sites, groups = groups)
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
    totals <- x@totals
    x@totals <- numeric(0)

    z <- methods::callNextMethod()

    if (is.null(dim(z))) {
      return(z)
    }

    if (!missing(i) & length(totals) > 0) {
      methods::initialize(x, z, totals = totals[i])
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
