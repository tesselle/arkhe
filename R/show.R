# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "AbundanceMatrix",
  definition = function(object) {
    m <- nrow(object)
    p <- ncol(object)
    mtx <- methods::as(object, "matrix")

    txt_dim <- sprintf("%d x %d", m, p)
    txt_mtx <- sprintf("<%s: %s>", class(object), txt_dim)

    cat(
      txt_mtx,
      utils::capture.output(mtx),
      sep = "\n"
    )
    invisible(object)
  }
)

setMethod(
  f = "show",
  signature = "AbundanceSummary",
  definition = function(object) {

    w <- getOption("width")
    txt_dots <- strrep("-", w)

    groups <- object@groups
    rows <- object@rows
    cols <- object@columns
    replicates <- object@replicates
    chronology <- object@chronology
    n <- length(groups)

    txt <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
      m <- rows[[i]]
      p <- cols[[i]]
      group <- groups[[i]]
      replic <- replicates[[i]]
      chrono <- chronology[i, , drop = TRUE]

      nrows <- ngettext(m, "Observation", "Observations")
      ncols <- ngettext(p, "Variable", "Variables")
      chro <- if (anyNA(chrono)) "unknown" else paste0(chrono, collapse = " - ")

      txt[[i]] <- paste(
        sprintf("%s %s", group, strrep("-", w - nchar(group) - 1)),
        sprintf("* %s: %d", nrows, m),
        sprintf("* %s: %d", ncols, p),
        sprintf("* %s: %s", "Replicated measurements", replic),
        sprintf("* %s: %s (year CE)", "Chronology", chro),
        sep = "\n"
      )
    }

    txt_dim <- sprintf("%d x %d", sum(rows), max(cols))
    txt_mtx <- sprintf("<%s: %s>", class(object), txt_dim)
    cat(txt_mtx, unlist(txt), txt_dots, sep = "\n")
    invisible(object)
  }
)
