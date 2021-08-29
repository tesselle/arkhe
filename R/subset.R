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
      dates_from <- x@dates_from
      dates_to <- x@dates_to
      if (!is_empty(samples)) samples <- samples[i]
      if (!is_empty(groups)) groups <- groups[i]
      if (!is_empty(dates_from)) dates_from <- dates_from[i]
      if (!is_empty(dates_to)) dates_to <- dates_to[i]
      methods::initialize(x, z, samples = samples, groups = groups,
                          dates_from = dates_from, dates_to = dates_to)
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

    if (!missing(i) & !is_empty(totals)) {
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
