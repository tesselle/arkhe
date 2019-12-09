# SHOW METHODS
#' @include AllClasses.R
NULL

# =============================================================== Logical matrix
setMethod(
  f = "show",
  signature = "IncidenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(
      sprintf("<IncidenceMatrix: %s>\n", object@id),
      sprintf("%d x %d presence/absence data matrix:\n", m, p)
    )
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "StratigraphicMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(
      sprintf("<StratigraphicMatrix: %s>\n", object@id),
      sprintf("%d x %d stratigraphic matrix:\n", m, p)
    )
    print(data)
  }
)

# =============================================================== Numeric matrix
setMethod(
  f = "show",
  signature = "CountMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(
      sprintf("<CountMatrix: %s>\n", object@id),
      sprintf("%d x %d absolute frequency matrix:\n", m, p)
    )
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "AbundanceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(
      sprintf("<AbundanceMatrix: %s>\n", object@id),
      sprintf("%d x %d relative frequency matrix:\n", m, p)
    )
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
    cat(
      sprintf("<OccurrenceMatrix: %s>\n", object@id),
      sprintf("%d x %d co-occurrence matrix:\n", m, p)
    )
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
    cat(
      sprintf("<SimilarityMatrix: %s>\n", object@id),
      sprintf("%d x %d (dis)similarity matrix (%s):\n", m, p, k)
    )
    print(data)
  }
)
