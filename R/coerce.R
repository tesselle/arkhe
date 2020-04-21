# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

#' Integer Vectors
#'
#' @param x A \code{\link{numeric}} vector to be coerced.
#' @details
#'  Numeric values are rounded to zero decimal places and then coerced to
#'  integer as by \code{\link{as.integer}}.
#' @return An \code{\link{integer}} vector.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
as_integer <- function(x) {
  as.integer(round(x, digits = 0))
}

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
#' @aliases as_matrix,ANY-method
setMethod(
  f = "as_matrix",
  signature = signature(from = "DataMatrix"),
  definition = function(from) {
    methods::as(from, "matrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_wide,ANY-method
setMethod(
  f = "as_wide",
  signature = signature(from = "DataMatrix"),
  definition = function(from) {
    methods::as(from, "data.frame")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_long,ANY-method
setMethod(
  f = "as_long",
  signature = signature(from = "DataMatrix"),
  definition = function(from) {
    data.frame(
      case = as.character(row(from, as.factor = TRUE)),
      type = as.character(col(from, as.factor = TRUE)),
      data = from@data,
      stringsAsFactors = FALSE
    )
  }
)

#' @export
#' @rdname coerce
#' @aliases as_features,DataMatrix-method
setMethod(
  f = "as_features",
  signature = "DataMatrix",
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
    mtx <- methods::as(from, "matrix")
    feat <- cbind.data.frame(SITE = from@row_names, coords, dates, mtx)
    # attr(feat, "epsg") <- epsg
    return(feat)
  }
)

# To an S3 matrix or data.frame ================================================
setAs(
  from = "DataMatrix",
  to = "matrix",
  def = function(from) {
    x <- matrix(data = from@data, nrow = nrow(from), ncol = ncol(from),
                byrow = FALSE, dimnames = dimnames(from))
    attr(x, "id") <- from@id
    x
  }
)
setAs(
  from = "DataMatrix",
  to = "data.frame",
  def = function(from) {
    x <- methods::as(from, "matrix")
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    attr(x, "id") <- from@id
    x
  }
)

# To CountMatrix ===============================================================
matrix2count <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  dim_names <- make_dimnames(from)
  .CountMatrix(
    data = as_integer(from),
    size = dim(from),
    row_names = dim_names[[1L]],
    column_names = dim_names[[2L]]
  )
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

# To AbundanceMatrix ===========================================================
matrix2frequency <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  dim_names <- make_dimnames(from)
  totals <- rowSums(from)
  freq <- from / totals
  .AbundanceMatrix(
    data = as.numeric(freq),
    size = dim(freq),
    row_names = dim_names[[1L]],
    column_names = dim_names[[2L]],
    totals = totals
  )
}
setAs(from = "matrix", to = "AbundanceMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "AbundanceMatrix", def = matrix2frequency)

# To SimilarityMatrix ==========================================================
matrix2similarity <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  size <- dim(from)
  dim_names <- make_dimnames(from)
  if (!all(Reduce("==", dim_names))) {
    var_names <- paste0("var", seq_len(size[[1L]]))
  } else {
    var_names <- dim_names[[1L]]
  }

  .SimilarityMatrix(
    data = as.integer(from),
    size = size,
    row_names = var_names,
    column_names = var_names,
    method = "unknown"
  )
}
setAs(from = "matrix", to = "SimilarityMatrix", def = matrix2similarity)
setAs(from = "data.frame", to = "SimilarityMatrix", def = matrix2similarity)

# To OccurrenceMatrix ==========================================================
matrix2occurrence <- function(from) {
  incid <- as_incidence(from)
  data <- as_matrix(incid)
  labels <- colnames(incid)
  p <- ncol(data)
  m <- nrow(data)

  # @param indices A length-two numeric vector
  # @param data A numeric or logical matrix
  fun <- function(indices, data) {
    sum(data[, indices[1]] + data[, indices[2]] == 2)
  }
  # Get all combinations of variables, taken 2 at a time
  combine <- utils::combn(seq_len(p), 2, simplify = TRUE)
  occurrence <- apply(X = combine, MARGIN = 2, FUN = fun, data = data) / m

  C <- matrix(data = 0, nrow = p, ncol = p)
  C[lower.tri(C, diag = FALSE)] <- occurrence
  C <- t(C)
  C[lower.tri(C, diag = FALSE)] <- occurrence

  .OccurrenceMatrix(
    id = incid@id,
    data = as.numeric(C),
    n = m,
    size = dim(C),
    row_names = labels,
    column_names = labels
  )
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
    counts <- from@data
    totals <- rowSums(from)
    freq <- counts / totals
    .AbundanceMatrix(
      id = from@id,
      data = freq,
      totals = totals,
      size = from@size,
      row_names = from@row_names,
      column_names = from@column_names,
      dates = from@dates
    )
  }
)
setAs(
  from = "AbundanceMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- from@data
    totals <- from@totals
    count <- as_integer(freq * totals)
    .CountMatrix(
      id = from@id,
      data = count,
      size = from@size,
      row_names = from@row_names,
      column_names = from@column_names,
      dates = from@dates
    )
  }
)

## To IncidenceMatrix ==========================================================
matrix2incidence <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  from <- from > 0
  dim_names <- make_dimnames(from)
  .IncidenceMatrix(
    data = as.logical(from),
    size = dim(from),
    row_names = dim_names[[1L]],
    column_names = dim_names[[2L]]
  )
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

Matrix2incidence <- function(from) {
  .IncidenceMatrix(
    id = from@id,
    data = from@data > 0,
    size = from@size,
    row_names = from@row_names,
    column_names = from@column_names,
    dates = from@dates
  )
}
setAs(from = "CountMatrix", to = "IncidenceMatrix", def = Matrix2incidence)
setAs(from = "AbundanceMatrix", to = "IncidenceMatrix", def = Matrix2incidence)

## To StratigraphicMatrix ======================================================
edges2matrix <- function(from) {
  from <- as.data.frame(from)
  # Get all layers
  layers <- unique(unlist(from))
  layers <- as.character(layers[order(layers)])
  # Coerce layers to factors
  edges <- lapply(X = from, FUN = factor, levels = layers)
  # Build adjacency matrix
  adj <- stats::xtabs(~ edges[[1]] + edges[[2]])
  adj <- matrix(data = as.logical(adj), nrow = length(layers),
                dimnames = list(lower = layers, upper = layers))

  .StratigraphicMatrix(
    data = as.logical(adj),
    size = c(length(layers), length(layers)),
    row_names = layers,
    column_names = layers
  )
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
