# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

# Interval =====================================================================
#' @export
#' @rdname confidence
#' @aliases confidence,numeric-method
setMethod(
  f = "confidence",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95, type = c("student", "normal")) {
    margin <- confidence_margin(object, level = level, type = type)
    interval <- mean(object) + margin * c(-1, 1)
    names(interval) <- c("lower", "upper")
    interval
  }
)

confidence_margin <- function(x, level = 0.95, type = c("student", "normal")) {
  z <- zscore(level = level, n = length(x), type = type)
  z * stardard_error(x)
}

stardard_error <- function(x) {
  stats::sd(x) / sqrt(length(x))
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

  results <- c(jack_mean, jack_bias, jack_error)
  names(results) <- c("mean", "bias", "error")
  results
}
