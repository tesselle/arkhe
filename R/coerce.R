# COERCION
#' @include AllClasses.R AllGenerics.R
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

#' Factors
#'
#' @param x A vector to be coerced.
#' @details
#'  Encodes a vector as a factor without sorting it into increasing order of
#'  \code{x} (preserves original ordering).
#' @return An \code{\link{factor}} object.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
as_factor <- function(x) {
  factor(x, levels = unique(x))
}

# To matrix or data.frame ======================================================
as_matrix <- function(from) {
  m <- from@values
  dim(m) <- from@size
  dimnames(m) <- list(from@row_names, from@column_names)
  m
}
as_dataframe <- function(from) {
  m <- as_matrix(from)
  m <- as.data.frame(m, stringsAsFactors = FALSE)
  m
}

#' @method as.matrix DataMatrix
#' @export
as.matrix.DataMatrix <- function(x, ...) {
  as_matrix(x)
}

#' @method as.data.frame DataMatrix
#' @export
as.data.frame.DataMatrix <- function(x, row.names = NULL, optional = FALSE,
                                     make.names = TRUE, ...,
                                     stringsAsFactors = default.stringsAsFactors()) {
  x <- as.data.frame(
    x = as.matrix(x),
    row.names = row.names,
    optional = optional,
    make.names = make.names,
    ...,
    stringsAsFactors = stringsAsFactors
  )
  x
}

setAs(from = "IntegerMatrix", to = "matrix", def = as_matrix)
setAs(from = "NumericMatrix", to = "matrix", def = as_matrix)
setAs(from = "LogicalMatrix", to = "matrix", def = as_matrix)

setAs(from = "IntegerMatrix", to = "data.frame", def = as_dataframe)
setAs(from = "NumericMatrix", to = "data.frame", def = as_dataframe)
setAs(from = "LogicalMatrix", to = "data.frame", def = as_dataframe)

# To CountMatrix ===============================================================
#' @export
#' @rdname coerce
#' @aliases as_count,ANY-method
setMethod(
  f = "as_count",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "CountMatrix")
)

matrix2count <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  dimnames(from) <- make_dimnames(from)
  .CountMatrix(
    size = dim(from),
    row_names = rownames(from),
    column_names = colnames(from),
    values = as_integer(from)
  )
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

# To AbundanceMatrix ===========================================================
#' @export
#' @rdname coerce
#' @aliases as_abundance,ANY-method
setMethod(
  f = "as_abundance",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "AbundanceMatrix")
)

matrix2frequency <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  dimnames(from) <- make_dimnames(from)
  AbundanceMatrix(
    data = from,
    nrow = nrow(from),
    ncol = ncol(from),
    dimnames = dimnames(from)
  )
}
setAs(from = "matrix", to = "AbundanceMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "AbundanceMatrix", def = matrix2frequency)

# CountMatrix <> AbundanceMatrix ===============================================
setAs(
  from = "CountMatrix",
  to = "AbundanceMatrix",
  def = function(from) {
    x <- AbundanceMatrix(
      data = from@values,
      nrow = nrow(from),
      ncol = ncol(from),
      dimnames = dimnames(from)
    )
    x
  }
)
setAs(
  from = "AbundanceMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- from@values
    totals <- from@totals
    count <- as_integer(freq * totals)
    x <- CountMatrix(
      data = count,
      nrow = nrow(from),
      ncol = ncol(from),
      dimnames = dimnames(from)
    )
    x
  }
)

# To SimilarityMatrix ==========================================================
#' @export
#' @rdname coerce
#' @aliases as_similarity,ANY-method
setMethod(
  f = "as_similarity",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "SimilarityMatrix")
)

matrix2similarity <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  dimnames(from) <- make_dimnames(from)
  SimilarityMatrix(
    data = from,
    nrow = nrow(from),
    ncol = ncol(from),
    dimnames = dimnames(from)
  )
}
setAs(from = "matrix", to = "SimilarityMatrix", def = matrix2similarity)
setAs(from = "data.frame", to = "SimilarityMatrix", def = matrix2similarity)

# To OccurrenceMatrix ==========================================================
#' @export
#' @rdname coerce
#' @aliases as_occurrence,ANY-method
setMethod(
  f = "as_occurrence",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "OccurrenceMatrix")
)

matrix2occurrence <- function(from) {
  incid <- as_incidence(from)
  data <- as.matrix(incid)
  labels <- colnames(incid)
  m <- nrow(incid)
  p <- ncol(incid)

  ij <- utils::combn(p, m = 2, simplify = TRUE)
  pair <- seq_len(ncol(ij))
  mtx <- matrix(data = 0, nrow = p, ncol = p)

  for (k in pair) {
    i <- ij[1, k]
    j <- ij[2, k]
    z <- sum(incid[, i] + incid[, j] == 2)
    mtx[i, j] <- mtx[j, i] <- z
  }

  .OccurrenceMatrix(
    size = c(p, p),
    row_names = labels,
    column_names = labels,
    values = as.integer(mtx),
    n = m
  )
}

setAs(from = "matrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "data.frame", to = "OccurrenceMatrix", def = matrix2occurrence)

setAs(from = "CountMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "AbundanceMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "IncidenceMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)

# To IncidenceMatrix ===========================================================
#' @export
#' @rdname coerce
#' @aliases as_incidence,ANY-method
setMethod(
  f = "as_incidence",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "IncidenceMatrix")
)

matrix2incidence <- function(from) {
  from <- data.matrix(from, rownames.force = NA)
  IncidenceMatrix(
    data = from > 0,
    nrow = nrow(from),
    ncol = ncol(from),
    dimnames = dimnames(from)
  )
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

Matrix2incidence <- function(from) {
  x <- IncidenceMatrix(
    data = from@values > 0,
    nrow = nrow(from),
    ncol = ncol(from),
    dimnames = dimnames(from)
  )
  x
}
setAs(from = "CountMatrix", to = "IncidenceMatrix", def = Matrix2incidence)
setAs(from = "AbundanceMatrix", to = "IncidenceMatrix", def = Matrix2incidence)

# To StratigraphicMatrix =======================================================
#' @export
#' @rdname coerce
#' @aliases as_stratigraphy,ANY-method
setMethod(
  f = "as_stratigraphy",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "StratigraphicMatrix")
)

edges2matrix <- function(from) {
  from <- as.data.frame(from)
  # Get all layers
  layers <- unique(unlist(from))
  layers <- as.character(layers[order(layers)])
  # Coerce layers to factors
  edges <- lapply(X = from, FUN = factor, levels = layers)
  # Build adjacency matrix
  adj <- stats::xtabs(~ edges[[1]] + edges[[2]])
  adj <- matrix(data = as.logical(adj), nrow = length(layers))

  .StratigraphicMatrix(
    size = dim(adj),
    row_names = layers,
    column_names = layers,
    values = as.logical(adj)
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

# Other ========================================================================
#' @export
#' @rdname coerce
#' @aliases as_long,DataMatrix-method
setMethod(
  f = "as_long",
  signature = signature(from = "DataMatrix"),
  definition = function(from, factor = FALSE) {
    x <- data.frame(
      case = as.character(row(from, as.factor = factor)),
      type = as.character(col(from, as.factor = factor)),
      value = from@values,
      stringsAsFactors = FALSE
    )
    if (factor) {
      x$case <- factor(x$case, levels = unique(x$case))
      x$type <- factor(x$type, levels = unique(x$type))
    }
    grp <- from@group_names
    if (length(grp) > 0) {
      x$group <- if (factor) factor(grp, levels = unique(grp)) else grp
    }
    x
  }
)

#' @export
#' @rdname coerce
#' @aliases as_features,DataMatrix-method
setMethod(
  f = "as_features",
  signature = "DataMatrix",
  definition = function(from) {
    # Get data from extra slots
    extra <- vector(mode = "list")
    slots <- methods::slotNames(from)
    for (s in slots) {
      x <- methods::slot(from, s)
      n <- length(x)
      if (n == nrow(from)) {
        extra <- c(extra, list(x))
        names(extra)[length(extra)] <- s
      }
    }

    mtx <- as.data.frame(from)
    cbind(mtx, extra)
  }
)
