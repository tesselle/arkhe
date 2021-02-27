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
  to <- data.matrix(from, rownames.force = NA)
  to <- as_integer(to)
  dim(to) <- dim(from)
  dimnames(to) <- make_dimnames(from)
  .CountMatrix(to)
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)
setAs(
  from = "AbundanceMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- methods::as(from, "matrix")
    totals <- from@totals
    counts <- as_integer(freq * totals)
    dim(counts) <- dim(freq)
    dimnames(counts) <- dimnames(freq)
    .CountMatrix(counts)
  }
)

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
  to <- data.matrix(from, rownames.force = NA)
  totals <- rowSums(to, na.rm = FALSE)
  to <- to / totals
  dim(to) <- dim(from)
  dimnames(to) <- make_dimnames(from)
  .AbundanceMatrix(to, totals = totals)
}
setAs(from = "matrix", to = "AbundanceMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "AbundanceMatrix", def = matrix2frequency)

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
  incid <- from > 0
  labels <- colnames(incid)
  m <- nrow(incid)
  p <- ncol(incid)

  ij <- utils::combn(p, m = 2, simplify = TRUE)
  pair <- seq_len(ncol(ij))
  mtx <- matrix(data = 0L, nrow = p, ncol = p)
  dimnames(mtx) <- list(colnames(from), colnames(from))

  for (k in pair) {
    i <- ij[1, k]
    j <- ij[2, k]
    z <- as.integer(sum(incid[, i] + incid[, j] == 2))
    mtx[i, j] <- mtx[j, i] <- z
  }

  .OccurrenceMatrix(mtx, total = m)
}

setAs(from = "matrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "data.frame", to = "OccurrenceMatrix", def = matrix2occurrence)

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
  to <- data.matrix(from, rownames.force = NA)
  to <- to > 0
  dim(to) <- dim(from)
  dimnames(to) <- make_dimnames(from)
  .IncidenceMatrix(to)
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

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

  .StratigraphicMatrix(adj)
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
#' @aliases as_long,matrix-method
setMethod(
  f = "as_long",
  signature = signature(from = "matrix"),
  definition = function(from, factor = FALSE) {
    x <- data.frame(
      row = as.vector(row(from, as.factor = factor)),
      column = as.vector(col(from, as.factor = factor)),
      value = as.vector(from),
      stringsAsFactors = FALSE
    )
    if (factor) {
      x$row <- as_factor(x$row)
      x$column <- as_factor(x$column)
    }
    x
  }
)

#' @export
#' @rdname coerce
#' @aliases as_long,ArchaeoMatrix-method
setMethod(
  f = "as_long",
  signature = signature(from = "ArchaeoMatrix"),
  definition = function(from, factor = FALSE) {
    x <- methods::callGeneric(from = methods::as(from, "matrix"),
                              factor = factor)

    sites <- from@sites
    if (length(sites) == 0) sites <- NA_character_
    x$site <- if (factor) as_factor(sites) else sites

    groups <- from@groups
    if (length(groups) == 0) groups <- NA_character_
    x$group <- if (factor) as_factor(groups) else groups

    x
  }
)

#' @export
#' @rdname coerce
#' @aliases as_features,ArchaeoMatrix-method
setMethod(
  f = "as_features",
  signature = "ArchaeoMatrix",
  definition = function(from) {
    df <- as.data.frame(from)

    sites <- from@sites
    if (length(sites) == 0) sites <- NA_character_
    df$site <- sites

    groups <- from@groups
    if (length(groups) == 0) groups <- NA_character_
    df$group <- groups

    df
  }
)
