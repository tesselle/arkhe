# SHOW METHODS
#' @include AllClasses.R
NULL

# Logical matrix ===============================================================
setMethod(
  f = "show",
  signature = "IncidenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d presence/absence data matrix:\n(%s)\n",
                m, p, object@id))
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "OccurrenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d co-occurrence matrix:\n(%s)\n", m, p, object@id))
    print(data)
  }
)

# Numeric matrix ===============================================================
setMethod(
  f = "show",
  signature = "CountMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d absolute frequency matrix:\n(%s)\n", m, p, object@id))
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "RelativeFrequencyMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d relative frequency matrix:\n(%s)\n", m, p, object@id))
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "SimilarityMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    k <- ifelse(length(object@method) == 0, "unknown", object@method)
    cat(sprintf("%d x %d (dis)similarity matrix (%s):\n(%s)\n",
                m, p, k, object@id))
    print(data)
  }
)
