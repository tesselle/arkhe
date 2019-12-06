# DEPRECATED METHODS

# ======================================================================= coerce
#' @export
#' @rdname deprecated
#' @aliases as_frequency,ANY-method
setMethod(
  f = "as_frequency",
  signature = signature(from = "ANY"),
  definition = function(from) {
    .Deprecated(new = "as_abundance")
    methods::as(from, "AbundanceMatrix")
  }
)
