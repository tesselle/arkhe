# PALETTES

#' Color Mapping
#'
#' @param x A vector of values.
#' @param palette A vector of colors or a color `function` that values will be
#'  mapped to (see [grDevices::colorRamp()]).
#' @param domain A [`numeric`] range or categorical data (according to `x`).
#' @param ordered A [`logical`] scalar: should the levels be treated as already
#'  in the correct order?
#' @param na The color to return for `NA` values.
#' @param ... Currently not used.
#' @return A [`character`] vector of colors.
#' @family palettes
#' @name palette_color
#' @rdname palette_color
NULL

#' @export
#' @rdname palette_color
palette_color_continuous <- function(x, palette = NULL, domain = NULL, na = "#DDDDDD", ...) {
  rng <- if (!is.null(domain)) range(domain, na.rm = TRUE) else range(x, na.rm = TRUE)
  x <- scale_range(x, from = rng)

  out <- x < 0 | x > 1
  if (any(out, na.rm = TRUE)) {
    x[out] <- NA
    warning("Some values were outside the color scale.", call. = FALSE)
  }

  OK <- !is.na(x)
  if (is.null(palette)) {
    palette <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
  }
  if (is.function(palette)) {
    palette <- palette(x[OK], ...)
  } else {
    palette <- grDevices::colorRamp(palette)(x[OK], ...)
  }

  col <- rep(na, length(x))
  col[OK] <- grDevices::rgb(palette, maxColorValue = 255)
  col
}

#' @export
#' @rdname palette_color
palette_color_discrete <- function(x, palette = NULL, domain = NULL,
                                   ordered = FALSE, na = "#DDDDDD", ...) {
  domain <- make_levels(x, levels = domain, ordered = ordered)
  n <- length(domain)
  x <- match(as.character(x), domain)

  OK <- !is.na(x)
  if (is.null(palette)) {
    palette <- grDevices::hcl.colors(n, "viridis")
  }
  if (is.function(palette)) {
    palette <- palette(n, ...)
  } else if (length(palette) < n) {
    palette <- grDevices::colorRampPalette(palette, ...)(n)
  }
  col <- palette[x]
  col[!OK] <- na
  col
}

make_levels <- function(x, levels = NULL, ordered = FALSE) {
  if (!is.null(levels)) return(make_levels(x = levels, ordered = ordered))

  if (is.null(x)) levels <- NULL
  else if (is.factor(x)) levels <- levels(x)
  else if (ordered) levels <- unique(x)
  else levels <- sort(unique(x))
  levels
}

#' Symbol Mapping
#'
#' @param x A vector of categorical values.
#' @param palette A vector of symbols.
#' @param domain A vector of categorical data.
#' @param ordered A [`logical`] scalar: should the levels be treated as already
#'  in the correct order?
#' @param ... Currently not used.
#' @return A vector of symbols.
#' @family palettes
#' @export
palette_shape <- function(x, palette = NULL, domain = NULL,
                          ordered = FALSE, ...) {
  domain <- make_levels(x, levels = domain, ordered = ordered)
  x <- match(as.character(x), domain)

  if (is.null(palette)) {
    n <- length(domain)
    if (n > 6) {
      warning("Consider specifying symbols manually: ",
              "more than 6 becomes difficult to discriminate.", call. = FALSE)
    }
    palette <- c(16, 17, 15, 3, 7, 8)[seq_len(n)]
  }
  symb <- palette[x]
  symb
}

#' Symbol Size Mapping
#'
#' @param x A vector of categorical values.
#' @param palette A [`numeric`] range.
#' @param domain A vector of categorical data.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector giving the amount by which plotting text and symbols
#'  should be magnified relative to the default (see [`par("cex")`][graphics::par()]).
#' @family palettes
#' @export
palette_size <- function(x, palette = NULL, domain = NULL, ...) {
  if (!is.numeric(x)) {
    warning("Discrete value supplied to continuous scale.", call. = FALSE)
    return(rep(1, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  pal <- if (!is.null(palette)) range(palette, na.rm = TRUE) else rng / max(x, na.rm = TRUE)
  dom <- if (!is.null(domain)) range(domain, na.rm = TRUE) else rng
  x <- scale_range(x, from = dom, to = pal)
  x
}
