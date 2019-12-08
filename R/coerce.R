# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname coerce
#' @aliases as_count,ANY-method
setMethod(
  f = "as_count",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "CountMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_abundance,ANY-method
setMethod(
  f = "as_abundance",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "AbundanceMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_incidence,ANY-method
setMethod(
  f = "as_incidence",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "IncidenceMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_occurrence,ANY-method
setMethod(
  f = "as_occurrence",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "OccurrenceMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_similarity,ANY-method
setMethod(
  f = "as_similarity",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "SimilarityMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_stratigraphy,ANY-method
setMethod(
  f = "as_stratigraphy",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "StratigraphicMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_features,Matrix-method
setMethod(
  f = "as_features",
  signature = "Matrix",
  definition = function(from) {
    # Spatial coordinates
    coords <- get_coordinates(from)

    if (nrow(coords) == 0) {
      coords <- matrix(data = rep(NA_real_, 3 * nrow(from)), ncol = 3,
                       dimnames = list(NULL, c("X", "Y", "Z")))
      coords <- as.data.frame(coords)
      message("No coordinates were set, NA generated.")
    }

    # Time coordinates
    dates <- get_dates(from)
    if (nrow(dates) == 0) {
      dates <- matrix(data = rep(NA_real_, 2 * nrow(from)), ncol = 2)
      dates <- as.data.frame(dates)
      message("No dates were set, NA generated.")
    }
    names(dates) <- c("DATE_VALUE", "DATE_ERROR")

    # XYZ_index <- !vapply(X = coords, FUN = anyNA, FUN.VALUE = logical(1))
    feat <- cbind.data.frame(SITE = rownames(from), coords, dates, from)
    # attr(feat, "epsg") <- epsg
    return(feat)
  }
)

# To data.frame ================================================================
setAs(
  from = "Matrix",
  to = "data.frame",
  def = function(from) {
    x <- methods::as(from, "matrix")
    x <- as.data.frame(x)
    attr(x, "id") <- from@id
    x
  }
)

## To CountMatrix ==================================================
matrix2count <- function(from) {
  data <- data.matrix(from, rownames.force = NA)
  data <- make_dimnames(data) # Force dimnames
  whole_numbers <- apply(
    X = data,
    MARGIN = 2,
    FUN = function(x) as.integer(round(x, digits = 0))
  )
  dimnames(whole_numbers) <- dimnames(data)
  .CountMatrix(whole_numbers, id = generate_uuid())
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

## To AbundanceMatrix ==================================================
matrix2frequency <- function(from) {
  data <- data.matrix(from)
  data <- make_dimnames(data) # Force dimnames
  totals <- rowSums(data)
  freq <- data / totals
  dimnames(freq) <- dimnames(data)
  .AbundanceMatrix(freq, totals = totals, id = generate_uuid())
}
setAs(from = "matrix", to = "AbundanceMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "AbundanceMatrix", def = matrix2frequency)

## To SimilarityMatrix =========================================================
matrix2similarity <- function(from) {
  data <- data.matrix(from)
  data <- make_colnames(data) # Force dimnames
  rownames(data) <- colnames(data)
  .SimilarityMatrix(data, method = "unknown", id = generate_uuid())
}
setAs(from = "matrix", to = "SimilarityMatrix", def = matrix2similarity)
setAs(from = "data.frame", to = "SimilarityMatrix", def = matrix2similarity)

## To OccurrenceMatrix =========================================================
matrix2occurrence <- function(from) {
  data <- if (isS4(from)) {
    methods::S3Part(from, strictS3 = TRUE, "matrix")
  } else {
    data.matrix(from)
  }
  data <- data > 0
  p <- ncol(data)
  m <- nrow(data)
  labels <- if (is.null(colnames(data))) {
    paste0("V", seq_len(p))
  } else {
    colnames(data)
  }

  # @param indices A length-two numeric vector
  # @param data A numeric or logical matrix
  fun <- function(indices, data) {
    sum(data[, indices[1]] + data[, indices[2]] == 2)
  }
  # Get all combinations of variables, taken 2 at a time
  combine <- utils::combn(seq_len(p), 2, simplify = TRUE)
  occurrence <- apply(X = combine, MARGIN = 2, FUN = fun, data = data) / m

  C <- matrix(data = FALSE, nrow = p, ncol = p, dimnames = list(labels, labels))
  C[lower.tri(C, diag = FALSE)] <- occurrence
  C <- t(C)
  C[lower.tri(C, diag = FALSE)] <- occurrence

  id <- ifelse(isS4(from), from@id, generate_uuid())
  .OccurrenceMatrix(C, id = id)
}

setAs(from = "matrix", to = "OccurrenceMatrix",
      def = matrix2occurrence)
setAs(from = "data.frame", to = "OccurrenceMatrix",
      def = matrix2occurrence)

setAs(from = "CountMatrix", to = "OccurrenceMatrix",
      def = matrix2occurrence)
setAs(from = "AbundanceMatrix", to = "OccurrenceMatrix",
      def = matrix2occurrence)
setAs(from = "IncidenceMatrix", to = "OccurrenceMatrix",
      def = matrix2occurrence)

## CountMatrix <> AbundanceMatrix ==============================================
setAs(
  from = "CountMatrix",
  to = "AbundanceMatrix",
  def = function(from) {
    counts <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    totals <- rowSums(counts)
    freq <- counts / totals
    .AbundanceMatrix(
      freq,
      totals = totals,
      id = from@id,
      dates = from@dates
    )
  }
)
setAs(
  from = "AbundanceMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    totals <- from@totals
    if (is_empty(totals))
      stop("Cannot calculate absolute frequencies (`totals` is empty).",
           call. = FALSE)
    count <- round(freq * totals, digits = 0)
    integer <- apply(
      X = count,
      MARGIN = 2,
      FUN = function(x) as.integer(round(x, digits = 0))
    )
    dimnames(integer) <- dimnames(freq)
    .CountMatrix(
      integer,
      id = from@id,
      dates = from@dates
    )
  }
)

## To IncidenceMatrix ==========================================================
matrix2incidence <- function(from) {
  data <- if (isS4(from)) {
    methods::S3Part(from, strictS3 = TRUE, "matrix")
  } else {
    data.matrix(from)
  }
  data <- make_dimnames(data) # Force dimnames
  data <- data > 0
  if (isS4(from)) {
    id <- from@id
    dates <- from@dates
  } else {
    id <- generate_uuid()
  }
  .IncidenceMatrix(
    data,
    id = id
  )
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

setAs(from = "CountMatrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "AbundanceMatrix", to = "IncidenceMatrix", def = matrix2incidence)

## To StratigraphicMatrix ======================================================
edges2matrix <- function(from) {
  from <- as.data.frame(from)
  # Get all layers
  layers <- unique(unlist(from))
  layers <- layers[order(layers)]
  # Coerce layers to factors
  edges <- lapply(X = from, FUN = factor, levels = layers)
  # Build adjacency matrix
  adj <- stats::xtabs(~ edges[[1]] + edges[[2]])
  adj <- matrix(data = as.logical(adj), nrow = length(layers),
                dimnames = list(lower = layers, upper = layers))

  .StratigraphicMatrix(adj, units = as.character(layers))
}
matrix2edges <- function(from) {
  edges <- matrix(data = NA, nrow = 0, ncol = 2)
  nodes <- seq_len(nrow(from))
  for (i in nodes) {
    to <- which(from[i, ])
    if (length(to) != 0) {
      e <- cbind(form = i, to = to)
      edges <- rbind(edges, e)
    }
  }
  return(edges)
}

setAs(from = "matrix", to = "StratigraphicMatrix", def = edges2matrix)
setAs(from = "data.frame", to = "StratigraphicMatrix", def = edges2matrix)
setAs(from = "list", to = "StratigraphicMatrix", def = edges2matrix)
