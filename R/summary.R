# SUMMARY
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname summary
#' @aliases summary,DataMatrix-method
setMethod(
  f = "summary",
  signature = signature(object = "DataMatrix"),
  definition = function(object, ...) {

    if (has_groups(object)) f <- get_groups(object)
    else f <- rep(x = "All data", times = nrow(object))
    groups <- split(x = seq_len(nrow(object)), f = f)
    z <- vector(mode = "list", length = length(groups))

    k <- 1
    for (i in groups) {
      data <- object[i, , drop = FALSE]
      z[[k]] <- list(
        group = unique(f[i]),
        dim = dim(data),
        stats = summary(as.matrix(data), ...)
      )
      k <- k + 1
    }

    methods::new("MatrixSummary", z)
  }
)
