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
#' @param spl_i An [`integer`].
#' @param grp_i An [`integer`].
#' @param dts_i An [`integer`].
#' @param tpq_i An [`integer`].
#' @param tap_i An [`integer`].
#' @param keep A [`character`] string.
#' @return A [`list`].
#' @author N. Frerebeau
#' @family utilities
#' @keywords internal utilities
#' @noRd
autodetect <- function(x, spl_i = NULL, grp_i = NULL, dts_i = NULL,
                       tpq_i = NULL, tap_i = NULL, keep = "numeric") {
  ## Default values
  samples <- rownames(x) %||% character(0)
  groups <- character(0)
  dts <- tpq <- taq <- integer(0)

  cols <- colnames(x)
  auto <- getOption("arkhe.autodetect") && !is.null(cols)
  index <- function(what, where) {
    grep(what, where, ignore.case = TRUE, value = FALSE)
  }
  ## Samples
  if (is.null(spl_i) && auto) spl_i <- index("^sample[s]{0,1}$", cols)
  if (has_length(spl_i, 1)) samples <- as.character(x[[spl_i]])

  ## Groups
  if (is.null(grp_i) && auto) grp_i <- index("^group[s]{0,1}$", cols)
  if (has_length(grp_i, 1)) groups <- as.character(x[[grp_i]])

  ## Dates
  if (is.null(dts_i) && auto) dts_i <- index("^date[s]{0,1}$", cols)
  if (has_length(dts_i, 1)) dts <- as.integer(x[[dts_i]])

  if (is.null(tpq_i) && auto) tpq_i <- index("tpq", cols)
  if (has_length(tpq_i, 1)) tpq <- as.integer(x[[tpq_i]])

  if (is.null(tap_i) && auto) tap_i <- index("taq", cols)
  if (has_length(tap_i, 1)) taq <- as.integer(x[[tap_i]])

  ## Drop extra columns (if any)
  drop <- c(spl_i, grp_i, dts_i, tpq_i, tap_i)
  data <- if (length(drop) > 0) x[, -drop, drop = FALSE] else x
  assert_filled(data)

  ## Remove non-numeric columns
  ok_num <- ok_log <- rep(FALSE, ncol(data))
  if (any(keep == "numeric")) ok_num <- detect(is_numeric, data)
  if (any(keep == "logical")) ok_log <- detect(is_logical, data)
  data <- data[, ok_num | ok_log, drop = FALSE]
  assert_filled(data)

  list(
    samples = samples,
    groups = groups,
    dates = dts,
    tpq = tpq,
    taq = taq,
    data = data
  )
}

# To CountMatrix ===============================================================
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
setAs(
  from = "matrix",
  to = "CountMatrix",
  def = function(from) {
    to <- data.matrix(from, rownames.force = NA)
    to <- as_integer(to)
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)

    samples <- rownames(to) %||% character(0)
    totals <- rowSums(to, na.rm = TRUE)
    .CountMatrix(to, samples = samples, totals = totals)
  }
)
setAs(
  from = "data.frame",
  to = "CountMatrix",
  def = function(from) {
    ## Get extra info
    extra <- autodetect(from, keep = "numeric")
    clean <- extra$data

    ## Build matrix
    mtx <- data.matrix(clean, rownames.force = NA)
    mtx <- methods::as(mtx, "CountMatrix")

    methods::initialize(mtx, samples = extra$samples, groups = extra$groups,
                        dates = extra$dates, tpq = extra$tpq, taq = extra$taq)
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
                 totals = totals, dates = from@dates, tpq = from@tpq,
                 taq = from@taq)
  }
)

# To CompositionMatrix =========================================================
#' @export
#' @rdname coerce
#' @aliases as_composition,ANY-method
setMethod(
  f = "as_composition",
  signature = signature(from = "ANY"),
  definition = function(from) {
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
    # to[is.nan(to)] <- 0 # Prevent division by zero?
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)

    samples <- rownames(to) %||% character(0)
    .CompositionMatrix(to, samples = samples, totals = totals)
  }
)
setAs(
  from = "data.frame",
  to = "CompositionMatrix",
  def = function(from) {
    ## Get extra info
    extra <- autodetect(from, keep = "numeric")
    clean <- extra$data

    ## Build matrix
    mtx <- data.matrix(clean, rownames.force = NA)
    mtx <- methods::as(mtx, "CompositionMatrix")

    .CompositionMatrix(mtx, samples = extra$samples, groups = extra$groups,
                       dates = extra$dates, tpq = extra$tpq, taq = extra$taq)
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
                       totals = totals, dates = from@dates, tpq = from@tpq,
                       taq = from@taq)
  }
)

# To IncidenceMatrix ===========================================================
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
setAs(
  from = "matrix",
  to = "IncidenceMatrix",
  def = function(from) {
    to <- data.matrix(from, rownames.force = NA)
    to <- to > 0
    dim(to) <- dim(from)
    dimnames(to) <- make_dimnames(from)

    samples <- rownames(to) %||% character(0)
    totals <- rowSums(to, na.rm = TRUE)
    .IncidenceMatrix(to, samples = samples, totals = totals)
  }
)
setAs(
  from = "data.frame",
  to = "IncidenceMatrix",
  def = function(from) {
    ## Get extra info
    extra <- autodetect(from, keep = c("numeric", "logical"))
    clean <- extra$data

    ## Build matrix
    mtx <- data.matrix(clean, rownames.force = NA)
    mtx <- methods::as(mtx, "IncidenceMatrix")

    .IncidenceMatrix(mtx, samples = extra$samples, groups = extra$groups,
                     dates = extra$dates, tpq = extra$tpq, taq = extra$taq)
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
                     totals = from@totals, dates = from@dates, tpq = from@tpq,
                     taq = from@taq)
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

    x$dates <- from@dates %||% NA_integer_
    x$tpq <- from@tpq %||% NA_integer_
    x$taq <- from@taq %||% NA_integer_

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
    x$dates <- from@dates %||% NA_integer_
    x$tpq <- from@tpq %||% NA_integer_
    x$taq <- from@taq %||% NA_integer_
    x
  }
)
