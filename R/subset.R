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
      totals <- x@totals
      dates <- x@dates
      tpq <- x@tpq
      taq <- x@taq
      if (!is_empty(samples)) samples <- samples[i]
      if (!is_empty(groups)) groups <- groups[i]
      if (!is_empty(totals)) totals <- totals[i]
      if (!is_empty(dates)) dates <- dates[i]
      if (!is_empty(tpq)) tpq <- tpq[i]
      if (!is_empty(taq)) taq <- taq[i]
      methods::initialize(x, z, samples = samples, groups = groups,
                          totals = totals, dates = dates, tpq = tpq, taq = taq)
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
