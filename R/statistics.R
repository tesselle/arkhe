# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

# Jaccknife ====================================================================
#' @export
#' @rdname jackknife
#' @aliases jackknife,numeric-method
setMethod(
  f = "jackknife",
  signature = c(x = "numeric"),
  definition = function(x, do, ...) {
    n <- length(x)
    hat <- do(x, ...)

    values <- vapply(
      X = seq_len(n),
      FUN = function(i, x, do, ...) {
        do(x[-i], ...)
      },
      FUN.VALUE = double(1),
      x = x, do = do, ...
    )
    .JackknifeVector(values, hat = hat)
  }
)

#' @export
#' @rdname jackknife
#' @aliases summary,JackknifeVector-method
setMethod(
  f = "summary",
  signature = c(object = "JackknifeVector"),
  definition = function(object, ...) {
    n <- length(object)
    jack_mean <- mean(object)
    jack_bias <- (n - 1) * (jack_mean - object@hat)
    jack_error <- sqrt(((n - 1) / n) * sum((object - jack_mean)^2))

    results <- c(jack_mean, jack_bias, jack_error)
    names(results) <- c("mean", "bias", "error")
    results
  }
)

# Bootstrap ====================================================================
#' @export
#' @describeIn bootstrap Samples randomly from the elements of `x` with
#'  replacement.
#' @aliases bootstrap,numeric-method
setMethod(
  f = "bootstrap",
  signature = c(x = "numeric"),
  definition = function(x, do, n, ...) {
    spl <- sample(x, size = length(x) * n, replace = TRUE)
    replicates <- t(matrix(spl, nrow = n))
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    .BootstrapVector(values)
  }
)

#' @export
#' @describeIn bootstrap Samples observations from a multinomial distribution.
#' @aliases bootstrap,integer-method
setMethod(
  f = "bootstrap",
  signature = c(x = "integer"),
  definition = function(x, do, n, ...) {
    size <- sum(x)
    replicates <- stats::rmultinom(n, size = size, prob = x / size)
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    .BootstrapVector(values)
  }
)

#' @export
#' @rdname bootstrap
#' @aliases summary,BootstrapVector-method
setMethod(
  f = "summary",
  signature = c(object = "BootstrapVector"),
  definition = function(object, level = 0.95, type = c("student", "normal"),
                        probs = c(0.25, 0.75), na.rm = FALSE, ...) {
    ## Confidence interval for the mean
    CI <- conf <- NULL
    if (!is.null(level)) {
      CI <- confidence_mean(object, level = level, type = type)
      conf <- c("lower", "upper")
    }

    ## Quantiles
    QU <- quant <- NULL
    if (!is.null(probs)) {
      QU <- stats::quantile(object, probs = probs, na.rm = na.rm, names = FALSE)
      quant <- sprintf("Q%02d", round(probs * 100, 0))
    }

    results <- c(
      min(object, na.rm = na.rm),
      mean(object, na.rm = na.rm),
      max(object, na.rm = na.rm),
      CI,
      QU
    )
    names(results) <- c("min", "mean", "max", conf, quant)
    results
  }
)

#' Confidence Interval for a Mean
#'
#' Computes the margin of errors of a confidence interval at a desired level of
#'  significance.
#' @param x A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param type A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (default) or
#'  "`normal`". Any unambiguous substring can be given.
#' @return A length-two [`numeric`] vector giving the margins of errors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
confidence_mean <- function(x, level = 0.95, type = c("student", "normal")) {
  ## Validation
  assert_type(x, "numeric")
  type <- match.arg(type, several.ok = FALSE)

  n <- length(x)
  z <- zscore(alpha = 1 - level, n = n, type = type)
  stardard_error <- stats::sd(x) / sqrt(n)

  margin <- z * stardard_error
  mean(x) + margin * c(-1, 1)
}

#' Confidence Interval for a Proportion
#'
#' Computes the margin of errors of a confidence interval at a desired level of
#'  significance.
#' @param x A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param type A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (default) or
#'  "`normal`". Any unambiguous substring can be given.
#' @return A [`numeric`] vector giving the margin of errors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
# confidence_prop <- function(x, level = 0.95, type = c("student", "normal")) {
#   ## Validation
#   assert_type(x, "numeric")
#   type <- match.arg(type, several.ok = FALSE)
#
#   n <- sum(x)
#   p <- x / n
#   z <- zscore(alpha = 1 - level, n = n, type = type)
#   stardard_error <- sqrt(p * (1 - p) / n)
#
#   margin <- z * stardard_error
#   margin
# }

zscore <- function(alpha, n, type = c("student", "normal")) {
  switch(
    type,
    normal = stats::qnorm(1 - alpha / 2), # Large sample size
    student = stats::qt(1 - alpha / 2, n - 1), # Small sample size
    stop(sprintf("There is no such type: %s", type), call. = FALSE)
  )
}
