# STATISTICS
#' @include AllGenerics.R
NULL

# HPDI =========================================================================
#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,numeric,numeric-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.954) {
    ## Compute density
    y <- y / sum(y)

    ## Order the sample (faster sorting with radix method)
    sorted <- sort(y, decreasing = TRUE, method = "radix")
    i <- min(which(cumsum(sorted) >= sum(y) * level))
    h <- sorted[[i]]
    idx <- which(y >= h)

    gap <- which(diff(idx) > 1)
    inf <- idx[c(1, gap + 1)]
    sup <- idx[c(gap, length(idx))]

    int <- mapply(FUN = seq, from = inf, to = sup,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    p <- vapply(X = int, FUN = function(i, y) { sum(y[i]) },
                FUN.VALUE = numeric(1), y = y)

    cbind(start = x[inf], end = x[sup], p = round(p, digits = 2))
  }
)

#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,numeric,missing-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "numeric", y = "missing"),
  definition = function(x, level = 0.954, ...) {
    ## Compute density
    d <- stats::density(x, ...)
    methods::callGeneric(x = d$x, y = d$y, level = level)
  }
)

# Credible interval ============================================================
#' @export
#' @rdname interval_credible
#' @aliases interval_credible,numeric-method
setMethod(
  f = "interval_credible",
  signature = "numeric",
  definition = function(x, level = 0.95) {
    ## Order the sample
    sorted <- sort(x, method = "radix") # Faster sorting with radix method

    ## Sample size
    N <- length(x)

    ## Number of data to be outside of the interval
    outside <- as.integer(N * (1 - level))
    inf <- seq(from = 1L, to = outside + 1L, by = 1L)
    sup <- seq(from = N - outside, to = N, by = 1L)

    ## Look for the shortest interval
    a <- sorted[sup]
    b <- sorted[inf]
    ind <- which.min(a - b)

    cbind(start = b[[ind]], end = a[[ind]], p = level)
  }
)

# Confidence interval ==========================================================
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

    if (is.function(f)) return(f(values))
    summary_bootstrap(values, hat)
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
  definition = function(object, do, ..., f = NULL) {
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

    if (is.function(f)) return(f(values))
    summary_jackknife(values, hat)
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
