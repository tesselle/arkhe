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

    ## Extra info
    obs <- ngettext(m, "Observation", "Observations")
    grp <- FALSE
    if (has_groups(object)) {
      grp <- length(unique(get_groups(object))) < length(get_samples(object))
    }
    chr <- "unknown"
    if (has_dates(object)) {
      chr <- paste0(range(get_dates(object), na.rm = TRUE, finite = TRUE),
                    collapse = " - ")
    }
    txt_dim <- sprintf("%d x %d", m, p)
    txt_mtx <- sprintf("<%s: %s>", class(object), txt_dim)
    txt_obs <- sprintf("* %s: %d", obs, m)
    txt_grp <- sprintf("* %s: %s", "Replicated measurements", as.character(grp))
    txt_chr <- sprintf("* %s: %s (year CE)", "Chronology", chr)

    cat(
      txt_mtx,
      # txt_obs,
      # txt_grp,
      # txt_chr,
      utils::capture.output(mtx),
      sep = "\n"
    )
    invisible(object)
  }
)
