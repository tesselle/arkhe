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
#'  Encodes a vector as a factor without sorting it into increasing (or
#'  decreasing if `reverse` is `TRUE`) order of `x` (preserves original
#'  ordering).
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
    .CountMatrix(to, samples = rownames(to))
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
    ## Add extra info
    spl <- rownames(to)
    grp <- character(0)
    if (getOption("arkhe.autodetect")) {
      extra_names <- colnames(extra)
      if (ncol(extra) > 0 & !is.null(extra_names)) {
        ## Samples
        spl_i <- grepl("sample", extra_names)
        if (sum(spl_i) == 1) spl <- extra[, spl_i, drop = TRUE]
        ## Groups
        grp_i <- grepl("group", extra_names)
        if (sum(grp_i) == 1) grp <- extra[, grp_i, drop = TRUE]
      }
    }
    .CountMatrix(to, samples = spl, groups = grp)
  }
)
setAs(
  from = "CompositionMatrix",
  to = "CountMatrix",
  def = function(from) {
    totals <- from@total
    counts <- as_integer(from * totals)
    dim(counts) <- dim(from)
    dimnames(counts) <- dimnames(from)
    .CountMatrix(counts, samples = from@samples, groups = from@groups)
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
#' @export
#' @rdname coerce
#' @aliases as_abundance,ANY-method
setMethod(
  f = "as_abundance",
  signature = signature(from = "ANY"),
  definition = function(from) {
    .Deprecated(new = "as_composition", old = "as_abundance")
    methods::as(from, "CompositionMatrix")
  }
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
    .CompositionMatrix(to, samples = rownames(to), total = totals)
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
    spl <- rownames(to)
    grp <- character(0)
    if (getOption("arkhe.autodetect")) {
      extra_names <- colnames(extra)
      if (ncol(extra) > 0 & !is.null(extra_names)) {
        ## Samples
        spl_i <- grepl("sample", extra_names)
        if (sum(spl_i) == 1) spl <- extra[, spl_i, drop = TRUE]
        ## Groups
        grp_i <- grepl("group", extra_names)
        if (sum(grp_i) == 1) grp <- extra[, grp_i, drop = TRUE]
      }
    }
    .CompositionMatrix(to, samples = spl, groups = grp, total = totals)
  }
)
setAs(
  from = "CountMatrix",
  to = "CompositionMatrix",
  def = function(from) {
    totals <- rowSums(from, na.rm = TRUE)
    to <- from / totals
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)
    .CompositionMatrix(to, samples = from@samples, groups = from@groups,
                       total = totals)
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
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)
    .IncidenceMatrix(to, samples = rownames(to))
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
    dim(to) <- dim(clean)
    dimnames(to) <- make_dimnames(clean)
    ## Add extra info
    spl <- rownames(to)
    grp <- character(0)
    if (getOption("arkhe.autodetect")) {
      extra_names <- colnames(extra)
      if (ncol(extra) > 0 & !is.null(extra_names)) {
        ## Samples
        spl_i <- grepl("sample", extra_names)
        if (sum(spl_i) == 1) spl <- extra[, spl_i, drop = TRUE]
        ## Groups
        grp_i <- grepl("group", extra_names)
        if (sum(grp_i) == 1) grp <- extra[, grp_i, drop = TRUE]
      }
    }
    .IncidenceMatrix(to, samples = spl, groups = grp)
  }
)
setAs(
  from = "AbundanceMatrix",
  to = "IncidenceMatrix",
  def = function(from) {
    incid <- from > 0
    dim(incid) <- dim(from)
    dimnames(incid) <- dimnames(from)
    .IncidenceMatrix(incid, samples = from@samples, groups = from@groups)
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
    x
  }
)
