# PLOT
NULL

#' Plotting Dimensions of Character Strings
#'
#' Convert string length in inch to number of (margin) lines.
#' @param x A [`character`] vector of string whose length is to be calculated.
#' @param ... Further parameter to be passed to [graphics::strwidth()]`, such as
#'  `cex`.
#' @return
#'  A [`numeric`] vector (maximum string width in units of margin lines).
#' @note For internal use only.
#' @family graphic tools
#' @keywords internal
#' @export
inch2line <- function(x, ...) {
  (max(graphics::strwidth(x, units = "inch", ...)) /
     graphics::par("cin")[2] + graphics::par("mgp")[2]) * graphics::par("cex")
}

#' Draw a Circle
#'
#' @param x,y A length-one [`numeric`] vector giving the coordinates of the
#'  center of the circle.
#' @param radius A length-one [`numeric`] vector giving the radius of the
#'  circle.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw the circle.
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @examples
#' \dontrun{
#' plot(NA, xlim = c(-1, 1), ylim = c(-1, 1),
#'      axes = FALSE, ann = FALSE, asp = 1)
#' circle(0, 0, 0.5)
#' }
#' @note For internal use only.
#' @author N. Frerebeau
#' @family graphic tools
#' @keywords internal
#' @export
circle <- function(x, y, radius, ..., n = 100) {
  angle.inc <- 2 * pi / n
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  graphics::polygon(xv, yv, ...)
}

#' Label Percentages
#'
#' @param x A [`numeric`] vector.
#' @param digits An [`integer`] indicating the number of decimal places.
#'  If `NULL` (the default), breaks will have the minimum number of digits
#'  needed to show the difference between adjacent values.
#' @param trim A [`logical`] scalar. If `FALSE` (the default), values are
#'  right-justified to a common width (see [base::format()]).
#' @note For internal use only.
#' @return A [`character`] vector.
#' @family graphic tools
#' @keywords internal
#' @export
label_percent <- function(x, digits = NULL, trim = FALSE) {
  i <- !is.na(x)
  y <- x[i]
  y <- abs(y) * 100
  y <- format(y, trim = trim, digits = digits)
  y <- paste0(y, "%")
  x[i] <- y
  x
}

#' Non-Overlapping Text Labels
#'
#' Optimize the location of text labels to minimize overplotting text.
#' @param x,y A [`numeric`] vector giving the x and y coordinates of a set of
#'  points. If `y` is `NULL`, an attempt is made to interpret `x` in a suitable
#'  way (see [grDevices::xy.coords()]).
#' @param labels A [`character`] vector or [`expression`] specifying the text
#'  to be written.
#' @param segment A [`logical`] scalar.
#' @param box A [`logical`] scalar.
#' @param ... Further arguments to be passed to [graphics::text()],
#'  particularly, character expansion, `cex` and color, `col`.
#' @return
#'  `label_auto()` is called it for its side-effects.
#' @note For internal use only.
#' @seealso [graphics::text()]
#' @author N. Frerebeau
#' @family graphic tools
#' @keywords internal
#' @export
label_auto <- function(x, y = NULL, ..., labels = seq_along(x),
                       segment = FALSE, box = FALSE) {
  ## Compute label positions
  xy <- grDevices::xy.coords(x, y)
  labs <- compute_labels(x = xy$x, y = xy$y, labels = labels, ...)

  xt <- labs$x
  yt <- labs$y
  wt <- labs$width
  ht <- labs$height

  ## Plot lines
  if (isTRUE(segment)) {
    for (i in seq_along(x)) {
      if (x[i] != xt[i] || y[i] != yt[i]) {
        graphics::lines(
          x = c(x[i], xt[i] - 0.5 * wt[i]),
          y = c(y[i], yt[i]),
          col = "grey"
        )
      }
    }
  }

  if (isTRUE(box)) {
    boxtext(x = xt, y = yt, width = wt, height = ht, ..., labels = labels)
  } else {
    shadowtext(x = xt, y = yt, ..., labels = labels)
  }
}

# Adapted from vegan::ordipointlabel() by Jari Oksanen
compute_labels <- function(x, y, ..., labels, cex = NULL,
                           font = NULL, vfont = NULL) {
  xy <- cbind.data.frame(x, y)

  em <- graphics::strwidth("m", cex = min(cex), font = font, vfont = vfont)
  ex <- graphics::strheight("x", cex = min(cex), font = font, vfont = vfont)
  ltr <- em * ex

  width <- graphics::strwidth(labels, cex = cex, font = font, vfont = vfont) + em
  height <- graphics::strheight(labels, cex = cex, font = font, vfont = vfont) + ex
  box <- cbind.data.frame(width, height)

  makeoff <- function(pos, lab) {
    cbind(
      c(0, 1, 0, -1, 0.9, 0.9, -0.9, -0.9)[pos] * lab[, 1] / 2,
      c(1, 0, -1, 0, 0.8, -0.8, -0.8, 0.8)[pos] * lab[, 2] / 2
    )
  }
  overlap <- function(xy1, off1, xy2, off2) {
    pmax(0, pmin(xy1[, 1] + off1[, 1]/2, xy2[, 1] + off2[, 1]/2) -
           pmax(xy1[, 1] - off1[, 1]/2, xy2[, 1] - off2[, 1]/2)) *
      pmax(0, pmin(xy1[, 2] + off1[, 2]/2, xy2[, 2] + off2[, 2]/2) -
             pmax(xy1[, 2] - off1[, 2]/2, xy2[, 2] - off2[, 2]/2))
  }

  n <- nrow(xy)
  j <- as.vector(stats::as.dist(row(matrix(0, n, n))))
  k <- as.vector(stats::as.dist(col(matrix(0, n, n))))

  maylap <- overlap(xy[j, ], 2 * box[j, ], xy[k, ], 2 * box[k, ]) > 0
  j <- j[maylap]
  k <- k[maylap]
  jk <- sort(unique(c(j, k)))

  nit <- min(48 * length(jk), 10000)
  pos <- rep(1, n)

  ## Simulated annealing
  fn <- function(pos) {
    off <- makeoff(pos, box)
    val <- sum(overlap(xy[j, ] + off[j, ], box[j, ], xy[k, ] + off[k, ], box[k, ]))
    val <- val / ltr + sum(pos > 1) * 0.1 + sum(pos > 4) * 0.1
  }
  gr <- function(pos) {
    take <- sample(jk, 1)
    pos[take] <- sample((1:8)[-pos[take]], 1)
    pos
  }
  sol <- stats::optim(par = pos, fn = fn, gr = gr, method = "SANN",
                      control = list(maxit = nit))

  coord <- xy + makeoff(sol$par, box)
  coord$width <- width
  coord$height <- height
  coord
}

shadowtext <- function(x, y, labels, ...,
                       theta = seq(0, 2 * pi, length.out = 50), r = 0.1,
                       cex = graphics::par("cex"), col = graphics::par("fg"),
                       bg = graphics::par("bg"), font = NULL, vfont = NULL,
                       xpd = TRUE) {

  xo <- r * graphics::strwidth("A", cex = cex, font = font, vfont = vfont, ...)
  yo <- r * graphics::strheight("A", cex = cex, font = font, vfont = vfont, ...)

  for (i in theta) {
    graphics::text(x = x + cos(i) * xo, y = y + sin(i) * yo, labels = labels,
                   col = bg, cex = cex, font = font, vfont = vfont,
                   xpd = xpd, ...)
  }

  graphics::text(x = x, y = y, labels = labels, col = col, cex = cex,
                 vfont = vfont, font = font, xpd = xpd, ...)
}

boxtext <- function(x, y, width, height, labels, ..., r = 0.1,
                    cex = graphics::par("cex"), col = graphics::par("fg"),
                    bg = graphics::par("bg"), font = NULL, vfont = NULL,
                    xpd = TRUE) {

  xo <- r * graphics::strwidth("M", cex = cex, font = font, vfont = vfont, ...)
  yo <- r * graphics::strheight("X", cex = cex, font = font, vfont = vfont, ...)

  .mapply(
    FUN = function(x, y, w, h, col, border, xpd) {
      roundrect(
        xleft = x - w - xo,
        ybottom = y - h - yo,
        xright = x + w + xo,
        ytop = y + h + yo,
        col = col,
        border = border,
        xpd = xpd
      )
    },
    dots = list(x = x, y = y, w = width * 0.5, h = height * 0.5,
                col = bg, border = col),
    MoreArgs = list(xpd = xpd)
  )
  graphics::text(x = x, y = y, labels = labels, col = col,
                 cex = cex, font = font, vfont = vfont, xpd = xpd, ...)
}

roundrect <- function(xleft, ybottom, xright, ytop, ...,
                      rounding = 0.25, n = 200) {

  XD <- YD <- min(c(xright - xleft, ytop - ybottom))
  xi <- rounding * XD
  yi <- rounding * YD

  ## Elliptic corners function
  elx <- function(from, to) xi * cos(seq(from, to, length.out = n / 4))
  ely <- function(from, to) yi * sin(seq(from, to, length.out = n / 4))

  ## x and y coordinates
  xc <- c(xright - xi + elx(0, pi / 2),
          xleft + xi + elx(pi / 2, pi),
          xleft + xi + elx(pi, 3 * pi / 2),
          xright - xi + elx(3 * pi / 2, 2 * pi))
  yc <- c(ytop - yi + ely(0, pi / 2),
          ytop - yi + ely(pi / 2, pi),
          ybottom + yi + ely(pi, 3 * pi / 2),
          ybottom + yi + ely(3 * pi / 2, 2 * pi))

  graphics::polygon(x = xc, y = yc, ...)
}
