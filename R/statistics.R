# STATISTICS
#' @include AllGenerics.R
NULL

# Interval =====================================================================
#' @export
#' @rdname confidence_mean
#' @aliases confidence_mean,numeric-method
setMethod(
  f = "confidence_mean",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95, type = c("student", "normal")) {
    z <- zscore(level = level, n = length(object), type = type)
    margin <- z * stats::sd(object) / sqrt(length(object))
    interval <- mean(object) + margin * c(-1, 1)
    names(interval) <- c("lower", "upper")
    interval
  }
)

#' @export
#' @rdname confidence_binomial
#' @aliases confidence_binomial,numeric-method
setMethod(
  f = "confidence_binomial",
  signature = c(object = "numeric"),
  definition = function(object, n, level = 0.95, method = "wald",
                        corrected = FALSE) {
    method <- match.arg(method, several.ok = FALSE)

    p <- object / n
    q <- 1 - p
    alpha <- 1 - level

    z <- stats::qnorm(1 - alpha / 2)
    margin <- z * sqrt(p * q / n)
    if (corrected) {
      margin <- margin + 1 / (2 * n) # Wald with continuity correction
    }

    interval <- c(lower = pmax(0, p - margin), upper = pmin(1, p + margin))
    interval
  }
)

#' @export
#' @rdname confidence_multinomial
#' @aliases confidence_multinomial,numeric-method
setMethod(
  f = "confidence_multinomial",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95, method = "wald",
                        corrected = FALSE) {
    method <- match.arg(method, several.ok = FALSE)

    n <- sum(object)
    f <- switch (
      method,
      wald = function(x) confidence_binomial(x, n = n, level = level,
                                             method = "wald",
                                             corrected = corrected)
    )

    interval <- vapply(X = object, FUN = f, FUN.VALUE = numeric(2))
    interval <- t(interval)
    rownames(interval) <- names(object)
    interval
  }
)

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
