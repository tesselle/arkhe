# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname jackknife
#' @aliases jackknife,numeric-method
setMethod(
  f = "jackknife",
  signature = c(x = "numeric"),
  definition = function(x, do, ...) {
    n <- length(x)
    hat <- do(x, ...)

    jack_values <- vapply(
      X = seq_len(n),
      FUN = function(i, x, do, ...) {
        do(x[-i], ...)
      },
      FUN.VALUE = double(1),
      x = x, do = do, ...
    )

    jack_mean <- mean(jack_values)
    jack_bias <- (n - 1) * (jack_mean - hat)
    jack_error <- sqrt(((n - 1) / n) * sum((jack_values - jack_mean)^2))

    results <- c(jack_mean, jack_bias, jack_error)
    names(results) <- c("mean", "bias", "error")
    results
  }
)

#' @export
#' @rdname bootstrap
#' @aliases bootstrap,numeric-method
setMethod(
  f = "bootstrap",
  signature = c(x = "numeric"),
  definition = function(x, do, probs = c(0.05, 0.95),
                        n = 1000, na.rm = FALSE, ...) {
    total <- sum(x)
    replicates <- stats::rmultinom(n, size = total, prob = x / total)
    boot_values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)

    Q <- stats::quantile(boot_values, probs = probs, na.rm = na.rm,
                         names = FALSE)
    quant <- sprintf("Q%02d", round(probs * 100, 0))

    results <- c(
      min(boot_values, na.rm = na.rm),
      mean(boot_values, na.rm = na.rm),
      max(boot_values, na.rm = na.rm),
      Q
    )
    names(results) <- c("min", "mean", "max", quant)
    results
  }
)
