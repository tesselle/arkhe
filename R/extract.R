# ACCESSORS
#' @include AllClasses.R
NULL

# =========================================================================== [[
#' Extract Parts of an Object
#'
#' @inheritParams subset
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
extract_slot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(i, choices = methods::slotNames(class_name),
                 several.ok = FALSE)
  data <- methods::slot(x, i)
  data
}

#' @export
#' @rdname subset
#' @aliases [[,SpaceTime-method
setMethod(
  f = "[[",
  signature = "SpaceTime",
  definition = function(x, i) {
    data <- extract_slot(x, i)
    if (i %in% c("dates", "coordinates"))
      data <- as.data.frame(data)
    data
  }
)
#' @export
#' @rdname subset
#' @aliases [[,CountMatrix-method
setMethod(
  f = "[[",
  signature = "CountMatrix",
  definition = extract_slot
)
#' @export
#' @rdname subset
#' @aliases [[,FrequencyMatrix-method
setMethod(
  f = "[[",
  signature = "FrequencyMatrix",
  definition = extract_slot
)
#' @export
#' @rdname subset
#' @aliases [[,IncidenceMatrix-method
setMethod(
  f = "[[",
  signature = "IncidenceMatrix",
  definition = extract_slot
)
#' @export
#' @rdname subset
#' @aliases [[,OccurrenceMatrix-method
setMethod(
  f = "[[",
  signature = "OccurrenceMatrix",
  definition = extract_slot
)
#' @export
#' @rdname subset
#' @aliases [[,SimilarityMatrix-method
setMethod(
  f = "[[",
  signature = "SimilarityMatrix",
  definition = extract_slot
)

# ====================================================================== Getters
#' @export
#' @rdname mutator
#' @aliases get_id,ANY-method
setMethod("get_id", "ANY", function(object) object@id)

#' @export
#' @rdname mutator
#' @aliases get_totals,FrequencyMatrix-method
setMethod("get_totals", "FrequencyMatrix", function(object) object@totals)

# Setters ======================================================================
#' @export
#' @rdname mutator
#' @aliases set_totals,FrequencyMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "FrequencyMatrix",
  definition = function(object, value) {
    object@totals <- value
    methods::validObject(object)
    object
  }
)
