# COERCION
#' @include AllClasses.R AllGenerics.R
NULL

#' Integer Vectors
#'
#' @param x A [`numeric`] vector to be coerced.
#' @details
#'  Numeric values are rounded to zero decimal places and then coerced to
#'  integer as by [as.integer()].
#' @return An [`integer`] vector.
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
#' @param reverse A [`logical`] scalar: should the order of factor
#'  levels be reversed? Useful for plotting.
#' @details
#'  Encodes a vector as a factor without sorting it (preserves original
#'  ordering or reverse it if `reverse` is `TRUE`).
#' @return An [`factor`] object.
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
as_factor <- function(x, reverse = FALSE) {
  lvl <- unique(x)
  if (reverse) {
    lvl <- rev(lvl)
  }
  factor(x, levels = lvl)
}

#' Autodetect Values
#'
#' @param x A [`data.frame`].
#' @param samples A [`character`] vector.
#' @param groups A [`character`] vector.
#' @param from An [`integer`] vector.
#' @param to An [`integer`] vector.
#' @return A [`list`].
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
autodetect <- function(x, samples = character(0), groups = character(0),
                       from = integer(0), to = integer(0)) {
  if (getOption("arkhe.autodetect")) {
    extra_names <- colnames(x)

    if (!is_empty(x) > 0 & !is_empty(extra_names)) {
      ## Samples
      spl_i <- grep("sample", extra_names, ignore.case = TRUE, value = FALSE)
      if (has_length(spl_i, 1)) samples <- as.character(x[[spl_i]])

      ## Groups
      grp_i <- grep("group", extra_names, ignore.case = TRUE, value = FALSE)
      if (has_length(grp_i, 1)) groups <- as.character(x[[grp_i]])

      ## TODO: Dates
      # from_i <- grep("from", extra_names, ignore.case = TRUE, value = FALSE)
      # if (has_length(from_i, 1)) from <- as.integer(x[[from_i]])
      # to_i <- grep("to", extra_names, ignore.case = TRUE, value = FALSE)
      # if (has_length(to_i, 1)) to <- as.integer(x[[to_i]])
    }
  }

  list(samples = samples, groups = groups, from = from, to = to)
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
setAs(
  from = "matrix",
  to = "CountMatrix",
  def = function(from) {
    to <- data.matrix(from, rownames.force = NA)
    to <- as_integer(to)
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)
    totals <- rowSums(to, na.rm = TRUE)
    .CountMatrix(to, samples = rownames(to), totals = totals)
  }
)
setAs(
  from = "data.frame",
  to = "CountMatrix",
  def = function(from) {
    ## Remove non-numeric columns
    ok <- detect(is_numeric, from)
    extra <- from[, !ok, drop = FALSE]

    ## Build matrix
    clean <- from[, ok, drop = FALSE]
    to <- data.matrix(clean, rownames.force = NA)
    to <- as_integer(to)
    dim(to) <- dim(clean)
    dimnames(to) <- make_dimnames(clean)
    totals <- rowSums(to, na.rm = TRUE)

    ## Add extra info
    info <- autodetect(extra, samples = rownames(to))

    .CountMatrix(to, samples = info$samples, groups = info$groups,
                 totals = totals, dates_from = info$from, dates_to = info$to)
  }
)
setAs(
  from = "CompositionMatrix",
  to = "CountMatrix",
  def = function(from) {
    totals <- from@totals
    counts <- as_integer(from * totals)
    dim(counts) <- dim(from)
    dimnames(counts) <- dimnames(from)
    .CountMatrix(counts, samples = from@samples, groups = from@groups,
                 totals = totals, dates_from = from@dates_from,
                 dates_to = from@dates_to)
  }
)

# To CompositionMatrix =========================================================
#' @export
#' @rdname coerce
#' @aliases as_composition,ANY-method
setMethod(
  f = "as_composition",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "CompositionMatrix")
)

setAs(
  from = "matrix",
  to = "CompositionMatrix",
  def = function(from) {
    to <- data.matrix(from, rownames.force = NA)
    totals <- rowSums(to, na.rm = TRUE)
    to <- to / totals
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)
    .CompositionMatrix(to, samples = rownames(to), totals = totals)
  }
)
setAs(
  from = "data.frame",
  to = "CompositionMatrix",
  def = function(from) {
    ## Remove non-numeric columns
    ok <- detect(is_numeric, from)
    extra <- from[, !ok, drop = FALSE]

    ## Build matrix
    clean <- from[, ok, drop = FALSE]
    to <- data.matrix(clean, rownames.force = NA)
    totals <- rowSums(to, na.rm = TRUE)
    to <- to / totals
    dim(to) <- dim(clean)
    dimnames(to) <- make_dimnames(clean)

    ## Add extra info
    info <- autodetect(extra, samples = rownames(to))

    .CompositionMatrix(to, samples = info$samples, groups = info$groups,
                       totals = totals, dates_from = info$from,
                       dates_to = info$to)
  }
)
setAs(
  from = "CountMatrix",
  to = "CompositionMatrix",
  def = function(from) {
    totals <- from@totals
    to <- from / totals
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)
    .CompositionMatrix(to, samples = from@samples, groups = from@groups,
                       totals = totals, dates_from = from@dates_from,
                       dates_to = from@dates_to)
  }
)

# To IncidenceMatrix ===========================================================
#' @export
#' @rdname coerce
#' @aliases as_incidence,ANY-method
setMethod(
  f = "as_incidence",
  signature = signature(from = "ANY"),
  definition = function(from) methods::as(from, "IncidenceMatrix")
)
setAs(
  from = "matrix",
  to = "IncidenceMatrix",
  def = function(from) {
    to <- data.matrix(from, rownames.force = NA)
    to <- to > 0
    totals <- rowSums(to, na.rm = TRUE)
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)
    .IncidenceMatrix(to, samples = rownames(to), totals = totals)
  }
)
setAs(
  from = "data.frame",
  to = "IncidenceMatrix",
  def = function(from) {
    ## Remove non-numeric columns
    ok <- detect(is_numeric, from) | detect(is_logical, from)
    extra <- from[, !ok, drop = FALSE]

    ## Build matrix
    clean <- from[, ok, drop = FALSE]
    to <- data.matrix(clean, rownames.force = NA)
    to <- to > 0
    totals <- rowSums(to, na.rm = TRUE)
    dim(to) <- dim(clean)
    dimnames(to) <- make_dimnames(clean)

    ## Add extra info
    info <- autodetect(extra, samples = rownames(to))

    .IncidenceMatrix(to, samples = info$samples, groups = info$groups,
                     totals = totals, dates_from = info$from,
                     dates_to = info$to)
  }
)
setAs(
  from = "AbundanceMatrix",
  to = "IncidenceMatrix",
  def = function(from) {
    incid <- from > 0
    dim(incid) <- dim(from)
    dimnames(incid) <- dimnames(from)
    .IncidenceMatrix(incid, samples = from@samples, groups = from@groups,
                     totals = from@totals, dates_from = from@dates_from,
                     dates_to = from@dates_to)
  }
)

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
  m <- nrow(incid)
  p <- ncol(incid)

  ij <- utils::combn(p, m = 2, simplify = TRUE)
  pair <- seq_len(ncol(ij))

  mtx <- matrix(data = 0L, nrow = p, ncol = p)
  labels <- colnames(incid)
  dimnames(mtx) <- list(labels, labels)

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
  definition = function(from, factor = FALSE, reverse = FALSE) {
    x <- data.frame(
      row = as.vector(row(from, as.factor = factor)),
      column = as.vector(col(from, as.factor = factor)),
      value = as.vector(from),
      stringsAsFactors = FALSE
    )
    if (factor) {
      x$row <- as_factor(x$row, reverse = reverse)
      x$column <- as_factor(x$column, reverse = reverse)
    }
    x
  }
)

#' @export
#' @rdname coerce
#' @aliases as_long,AbundanceMatrix-method
setMethod(
  f = "as_long",
  signature = signature(from = "AbundanceMatrix"),
  definition = function(from, factor = FALSE, reverse = FALSE) {
    x <- methods::callGeneric(from = methods::as(from, "matrix"),
                              factor = factor, reverse = reverse)

    samples <- from@samples %||% NA_character_
    x$samples <- if (factor) as_factor(samples, reverse = reverse) else samples

    groups <- from@groups %||% NA_character_
    x$groups <- if (factor) as_factor(groups, reverse = reverse) else groups

    x$from <- from@dates_from %||% NA_integer_
    x$to <- from@dates_to %||% NA_integer_

    x
  }
)

#' @export
#' @rdname coerce
#' @aliases as_features,AbundanceMatrix-method
setMethod(
  f = "as_features",
  signature = "AbundanceMatrix",
  definition = function(from) {
    x <- as.data.frame(from)
    x$samples <- from@samples %||% NA_character_
    x$group <- from@groups %||% NA_character_
    x$from <- from@dates_from %||% NA_integer_
    x$to <- from@dates_to %||% NA_integer_
    x
  }
)
