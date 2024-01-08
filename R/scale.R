# SCALE
NULL

#' Rescale Continuous Vector
#'
#' Rescales continuous vector to have specified minimum and maximum.
#' @param x A [`numeric`] vector.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @note For internal use only.
#' @return A [`numeric`] vector.
#' @family scales
#' @export
scale_range <- function(x, to = c(0, 1), from = range(x, finite = TRUE)) {
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

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
#' @family palette
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
  } else {
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
