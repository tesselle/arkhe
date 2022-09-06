# STATISTICS
#' @include AllGenerics.R
NULL

# Interval =====================================================================
#' @export
#' @rdname confidence
#' @aliases confidence,numeric-method
setMethod(
  f = "confidence",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95, type = c("student", "normal")) {
    margin <- confidence_mean(object, level = level, type = type)
    interval <- mean(object) + margin * c(-1, 1)
    names(interval) <- c("lower", "upper")
    interval
  }
)

confidence_mean <- function(x, level = 0.95, type = c("student", "normal")) {
  z <- zscore(level = level, n = length(x), type = type)
  z * stats::sd(x) / sqrt(length(x))
}

confidence_prop <- function(x, level = 0.95, type = c("student", "normal")) {
  n <- sum(x)
  p <- x / n

  z <- zscore(level = level, n = length(x), type = type)
  z * sqrt(p * (1 - p) / n)
}

zscore <- function(level, n, type = c("student", "normal")) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)

  alpha <- 1 - level
  switch(
    type,
    normal = stats::qnorm(1 - alpha / 2), # Large sample size
    student = stats::qt(1 - alpha / 2, df = n - 1), # Small sample size
  )
}

# Bootstrap ====================================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,numeric-method
setMethod(
  f = "bootstrap",
  signature = c(object = "numeric"),
  definition = function(object, do, n, ..., f = NULL) {
    hat <- do(object, ...)

    spl <- sample(object, size = length(object) * n, replace = TRUE)
    replicates <- t(matrix(spl, nrow = n))
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    values <- if (is.function(f)) f(values) else summary_bootstrap(values, hat)
    values
  }
)

summary_bootstrap <- function(x, hat) {
  n <- length(x)
  boot_mean <- mean(x)
  boot_bias <- boot_mean - hat
  boot_error <- stats::sd(x)

  results <- c(hat, boot_mean, boot_bias, boot_error)
  names(results) <- c("original", "mean", "bias", "error")
  results
}

# Jaccknife ====================================================================
#' @export
#' @rdname jackknife
#' @aliases jackknife,numeric-method
setMethod(
  f = "jackknife",
  signature = c(object = "numeric"),
  definition = function(object, do, ...) {
    n <- length(object)
    hat <- do(object, ...)

    values <- vapply(
      X = seq_len(n),
      FUN = function(i, x, do, ...) {
        do(x[-i], ...)
      },
      FUN.VALUE = double(1),
      x = object, do = do, ...
    )
    values <- summary_jackknife(values, hat)
    values
  }
)

summary_jackknife <- function(x, hat) {
  n <- length(x)
  jack_mean <- mean(x)
  jack_bias <- (n - 1) * (jack_mean - hat)
  jack_error <- sqrt(((n - 1) / n) * sum((x - jack_mean)^2))

  results <- c(hat, jack_mean, jack_bias, jack_error)
  names(results) <- c("original", "mean", "bias", "error")
  results
}
