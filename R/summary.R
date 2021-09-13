# SUMMARY
#' @include AllClasses.R
NULL

#' @export
#' @rdname summary
#' @aliases summary,AbundanceMatrix-method
setMethod(
  f = "summary",
  signature = "AbundanceMatrix",
  definition = function(object, ...) {
    ## Get groups (if any)
    m <- nrow(object)
    groups <- get_groups(object) %||% rep("All data", m)
    index <- split(seq_len(m), groups)

    n <- length(index)
    rows <- integer(n)
    cols <- integer(n)
    repl <- logical(n)
    chrono <- matrix(data = NA_integer_, nrow = n, ncol = 2)

    for (i in seq_len(n)) {
      k <- index[[i]]
      data <- object[k, , drop = FALSE]

      rows[[i]] <- nrow(data)
      cols[[i]] <- ncol(data)
      repl[[i]] <- length(unique(get_samples(data))) < nrow(data)
      dates <- c(get_tpq(data), get_taq(data), get_dates(data))
      if (!is_empty(dates)) chrono[i, ] <- range(dates, finite = TRUE)
    }

    .AbundanceSummary(
      groups = sort(unique(groups)),
      rows = rows,
      columns = cols,
      replicates = repl,
      chronology = chrono
    )
  }
)
