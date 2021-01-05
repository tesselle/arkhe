# CORRESPONDENCE ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname correspondence
#' @aliases ca,CountMatrix-method
setMethod(
  f = "ca",
  signature = signature(object = "CountMatrix"),
  definition = function(object, n = 5, sup_rows = NULL, sup_columns = NULL) {
    # Coerce to matrix
    object <- as.matrix(object)

    # Subset
    is_row_sup <- is_supplementary(sup_rows, nrow(object))
    is_col_sup <- is_supplementary(sup_columns, ncol(object))
    N <- object[!is_row_sup, !is_col_sup]

    # Dimension of the solution
    ndim <- min(n, dim(N) - 1)
    keep_dim <- seq_len(ndim)

    # Grand total
    total <- sum(N, na.rm = FALSE)
    # Relative frequencies
    P <- N / total

    # Calcul des marges
    w_row <- rowSums(P, na.rm = FALSE)
    w_col <- colSums(P, na.rm = FALSE)
    W_row <- diag(1 / w_row)
    W_col <- diag(1 / w_col)

    # /!\ Important: we need to clean the data before processing
    # Empty rows/columns must be removed to avoid error in svd()
    if (any(w_row == 0))
      stop("Empty rows detected.", call. = FALSE)
    if (any(w_col == 0))
      stop("Empty columns detected.", call. = FALSE)

    # Calcul des écarts à l'indépendance
    M <- P - w_row %*% t(w_col)
    # Matrix of standardized residuals
    S <- sqrt(W_row) %*% M %*% sqrt(W_col)

    # Singular Value Decomposition
    D <- svd(S)
    U <- D$u[, keep_dim]
    V <- D$v[, keep_dim]
    sv <- D$d # Singular values

    # Standard coordinates
    coord_row <- sqrt(W_row) %*% U
    coord_col <- sqrt(W_col) %*% V

    # Distance to centroide
    dist_row <- sqrt(rowSums(S^2) / w_row)
    dist_col <- sqrt(colSums(S^2) / w_col)

    # Inertias
    inertia_row <- w_row * dist_row^2
    inertia_col <- w_col * dist_col^2

    # Supplementary points
    coord_row_sup <- coord_col_sup <- matrix(data = 0, nrow = 0, ncol = ndim)
    if (any(is_row_sup)) {
      extra_row <- object[is_row_sup, !is_col_sup, drop = FALSE]
      n_sup <- nrow(extra_row)
      row_sup <- extra_row / rowSums(extra_row)
      sv_row_sup <- matrix(data = sv[keep_dim], nrow = n_sup, ncol = ndim, byrow = TRUE)
      # Standard coordinates
      coord_row_sup <- crossprod(t(row_sup) - w_col, coord_col) / sv_row_sup
      # Distances
      dist_row_sup <- sqrt(rowSums(t((t(row_sup) - w_col) / sqrt(w_col))^2))
      dist_row <- c(dist_row, dist_row_sup)
      # Inertias
      inertia_row <- c(inertia_row, rep(NA_real_, n_sup))
    }
    if (any(is_col_sup)) {
      extra_col <- object[!is_row_sup, is_col_sup, drop = FALSE]
      n_sup <- ncol(extra_col)
      col_sup <- t(t(extra_col) / colSums(extra_col))
      sv_col_sup <- matrix(data = sv[keep_dim], nrow = n_sup, ncol = ndim, byrow = TRUE)
      # Standard coordinates
      coord_col_sup <- crossprod(col_sup - w_row, coord_row) / sv_col_sup
      # Distances
      dist_col_sup <- sqrt(colSums(((col_sup - w_row) / sqrt(w_row))^2))
      dist_col <- c(dist_col, dist_col_sup)
      # Inertia
      inertia_col <- c(inertia_col, rep(NA_real_, n_sup))
    }

    .CA(
      dimension = as.integer(ndim),
      row_names = rownames(object),
      row_coordinates = rbind(coord_row, coord_row_sup),
      row_distances = dist_row,
      row_inertias = inertia_row,
      row_masses = w_row,
      row_supplement = is_row_sup,
      column_names = colnames(object),
      column_coordinates = rbind(coord_col, coord_col_sup),
      column_distances = dist_col,
      column_inertias = inertia_col,
      column_masses = w_col,
      column_supplement = is_col_sup,
      singular_values = sv
    )
  }
)

#' @param index A \code{\link{numeric}} vector.
#' @param n An \code{\link{integer}} value.
#' @return A \code{\link{logical}} vector.
#' @keywords internal
#' @noRd
is_supplementary <- function(index, n) {
  x <- rep(FALSE, times = n)

  if (!is.null(index) && (!is.numeric(index) & !is.logical(index))) {
    arg <- deparse(substitute(index))
    msg <- sprintf("%s must be a numeric vector of indices.", sQuote(arg))
    stop(msg, call. = FALSE)
  } else {
    x[index] <- TRUE
  }

  x
}

#' @export
#' @rdname correspondence
#' @aliases predict,CA-method
setMethod(
  f = "predict",
  signature = signature(object = "CA"),
  definition = function(object, data, margin = 1) {
    # Coerce to matrix
    data <- as.matrix(data)
    if (margin == 1) data <- data / rowSums(data)
    if (margin == 2) data <- t(data) / colSums(data)

    # TODO: keep only matching columns
    # index <- which(rownames(data) %in% object@column_names)
    # data <- data[, index]

    # Get standard coordinates (SVD)
    std <- get_coordinates(object, margin = 3 - margin,
                           standard = TRUE, sup = FALSE)

    coords <- data %*% as.matrix(std)
    as.data.frame(coords)
  }
)

#' @export
#' @rdname correspondence
#' @aliases get_coordinates,CA-method
setMethod(
  f = "get_coordinates",
  signature = signature(object = "CA"),
  definition = function(object, margin = 1, standard = FALSE,
                        sup = TRUE, sup_name = ".sup") {

    if (margin == 1) {
      coords <- object@row_coordinates
      suppl <- object@row_supplement
      name <- object@row_names
    }
    if (margin == 2) {
      coords <- object@column_coordinates
      suppl <- object@column_supplement
      name <- object@column_names
    }

    keep_dim <- seq_len(ncol(coords))
    if (!standard) {
      sv <- object@singular_values
      coords <- coords %*% diag(sv[keep_dim])
    }

    coords <- as.data.frame(coords)
    rownames(coords) <- name
    colnames(coords) <- paste0("CA", keep_dim)

    if (sup) {
      coords[[sup_name]] <- suppl
    } else {
      coords <- coords[!suppl, ]
    }

    coords
  }
)

#' @export
#' @rdname correspondence
#' @aliases get_inertia,CA-method
setMethod(
  f = "get_inertia",
  signature = signature(object = "CA"),
  definition = function(object, margin = 1) {
    if (margin == 1) {
      i <- object@row_inertias
      names(i) <- object@row_names
    }
    if (margin == 2) {
      i <- object@column_inertias
      names(i) <- object@column_names
    }
    i
  }
)

#' @export
#' @rdname correspondence
#' @aliases get_distances,CA-method
setMethod(
  f = "get_distances",
  signature = signature(object = "CA"),
  definition = function(object, margin = 1) {
    if (margin == 1) {
      d <- object@row_distances
      names(d) <- object@row_names
    }
    if (margin == 2) {
      d <- object@column_distances
      names(d) <- object@column_names
    }
    d
  }
)

#' @export
#' @rdname correspondence
#' @aliases get_contributions,CA-method
setMethod(
  f = "get_contributions",
  signature = signature(object = "CA"),
  definition = function(object, margin = 1) {
    coords <- get_coordinates(object, margin = margin, standard = FALSE,
                              sup = FALSE)

    sv <- object@singular_values
    if (margin == 1) masses <- object@row_masses
    if (margin == 2) masses <- object@column_masses

    coords <- as.matrix(coords)
    keep_dim <- seq_len(ncol(coords))
    contrib <- t(t(coords^2 * masses) / sv[keep_dim]^2) * 100
    as.data.frame(contrib)
  }
)

#' @export
#' @rdname correspondence
#' @aliases get_eigenvalues,CA-method
setMethod(
  f = "get_eigenvalues",
  signature = signature(object = "CA"),
  definition = function(object) {
    eig <- object@singular_values^2 # Eigenvalues
    pvar <- eig / sum(eig) * 100 # Percentage of variance
    cvar <- cumsum(pvar) # Cumulative percentage of variance

    data.frame(
      eigenvalues = eig,
      percentage = pvar,
      cumulative = cvar,
      row.names =  paste0("CA", seq_along(eig))
    )
  }
)
