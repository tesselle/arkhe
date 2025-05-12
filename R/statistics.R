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
    ## Validation
    assert_scalar(level, "numeric")

    z <- zscore(level = level, n = length(object), type = type)
    margin <- z * stats::sd(object) / sqrt(length(object))
    interval <- mean(object) + margin * c(-1, 1)
    names(interval) <- c("lower", "upper")
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

#' @export
#' @rdname confidence_binomial
#' @aliases confidence_binomial,numeric-method
setMethod(
  f = "confidence_binomial",
  signature = c(object = "numeric"),
  definition = function(object, n, level = 0.95, method = "wald",
                        corrected = FALSE) {
    ## Validation
    assert_scalar(level, "numeric")
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
    ## Validation
    assert_scalar(level, "numeric")
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

#' @export
#' @rdname confidence_bootstrap
#' @aliases confidence_bootstrap,numeric-method
setMethod(
  f = "confidence_bootstrap",
  signature = c(object = "numeric"),
  definition = function(object, level = 0.95,
                        type = c("basic", "normal", "student", "percentiles"),
                        t0 = NULL, var_t0 = NULL, var_t = NULL, ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)
    assert_scalar(level, "numeric")
    if (is.null(var_t)) {
      fin <- which(is.finite(object))
    } else {
      assert_length(var_t, length(object))
      assert_type(var_t, "numeric")
      fin <- which(is.finite(object) & is.finite(var_t))
      var_t <- var_t[fin]
    }
    object <- object[fin]

    if (type == "percentiles" | type == "basic") {
      ## Percentile confidence interval
      # conf <- stats::quantile(object, probs = probs, names = FALSE)
      probs <- (1 + c(-level, level)) / 2
      conf <- qq(object, probs)
    }
    if (type == "basic") {
      ## Basic bootstrap confidence limits (Davison & Hinkley, 1997)
      assert_scalar(t0, "numeric")
      conf <- 2 * t0 - rev(conf)
    }
    if (type == "normal") {
      ## Normal approximation (Davison & Hinkley, 1997)
      assert_scalar(t0, "numeric")
      if (is.null(var_t0)) var_t0 <- stats::var(object)

      bias <- mean(object) - t0
      zscore <- sqrt(var_t0) * stats::qnorm((1 + level) / 2)
      conf <- c(t0 - bias - zscore, t0 - bias + zscore)
    }
    if (type == "student") {
      ## Studentized bootstrap confidence limits (Davison & Hinkley, 1997)
      assert_scalar(t0, "numeric")
      assert_scalar(var_t0, "numeric")

      probs <- (1 + c(level, -level)) / 2
      zscore <- (object - t0) / sqrt(var_t)
      conf <- t0 - sqrt(var_t0) * qq(zscore, probs)
    }

    names(conf) <- c("lower", "upper")
    conf
  }
)

# Copy non-exported from boot
# Davison and Hinkley (1997), eq. 5.8
qq <- function(x, alpha) {
  x <- x[is.finite(x)]
  R <- length(x)
  rk <- (R + 1) * alpha
  if (!all(rk > 1 & rk < R))
    warning(tr_("Extreme order statistics used as endpoints."), call. = FALSE)
  k <- trunc(rk)
  inds <- seq_along(k)
  out <- inds
  kvs <- k[k > 0 & k < R]
  tstar <- sort(x, partial = sort(union(c(1, R), c(kvs, kvs + 1))))
  ints <- (k == rk)
  if (any(ints)) out[inds[ints]] <- tstar[k[inds[ints]]]
  out[k == 0] <- tstar[1L]
  out[k == R] <- tstar[R]
  not <- function(v) xor(rep(TRUE,length(v)), v)
  temp <- inds[not(ints) & k != 0 & k != R]
  temp1 <- stats::qnorm(alpha[temp])
  temp2 <- stats::qnorm(k[temp] / (R + 1))
  temp3 <- stats::qnorm((k[temp] + 1) / (R + 1))
  tk <- tstar[k[temp]]
  tk1 <- tstar[k[temp] + 1L]
  out[temp] <- tk + (temp1 - temp2) / (temp3 - temp2) * (tk1 - tk)
  out
}

# Bootstrap ====================================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,numeric-method
setMethod(
  f = "bootstrap",
  signature = c(object = "numeric"),
  definition = function(object, do, n, ..., f = NULL, level = 0.95,
                        interval = c("basic", "normal", "percentiles")) {
    interval <- match.arg(interval, several.ok = FALSE)

    hat <- do(object, ...)

    spl <- sample(object, size = length(object) * n, replace = TRUE)
    replicates <- t(matrix(spl, nrow = n))
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)

    if (is.function(f)) return(f(values))
    summary_bootstrap(values, hat, level = level, interval = interval)
  }
)

summary_bootstrap <- function(x, hat, level = 0.95, interval = "basic") {
  n <- length(x)
  boot_mean <- mean(x)
  boot_bias <- boot_mean - hat
  boot_error <- stats::sd(x)

  ci <- confidence_bootstrap(x, level = level, t0 = hat, type = interval)
  results <- c(hat, boot_mean, boot_bias, boot_error, ci)
  names(results) <- c("original", "mean", "bias", "error", "lower", "upper")
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

# Resample =====================================================================
#' @export
#' @rdname resample_uniform
#' @aliases resample_uniform,numeric-method
setMethod(
  f = "resample_uniform",
  signature = c(object = "numeric"),
  definition = function(object, n, size = length(object), replace = FALSE, ...) {
    spl <- replicate(
      n = n,
      expr = sample(object, size = size, replace = replace),
      simplify = FALSE
    )
    do.call(rbind, spl)
  }
)

#' @export
#' @rdname resample_multinomial
#' @aliases resample_multinomial,numeric-method
setMethod(
  f = "resample_multinomial",
  signature = c(object = "numeric"),
  definition = function(object, n, size = sum(object), ...) {
    prob <- object / sum(object)
    t(stats::rmultinom(n, size = size, prob = prob))
  }
)
