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
  definition = function(x, do, level = 0.95, type = c("student", "normal"),
                        probs = c(0.25, 0.75), n = 1000, na.rm = FALSE, ...) {
    total <- sum(x)
    replicates <- stats::rmultinom(n, size = total, prob = x / total)
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)

    CI <- confidence_mean(values, level = level, type = type)

    QU <- quant <- NULL
    if (!is.null(probs)) {
      QU <- stats::quantile(values, probs = probs, na.rm = na.rm, names = FALSE)
      quant <- sprintf("Q%02d", round(probs * 100, 0))
    }

    results <- c(
      min(values, na.rm = na.rm),
      mean(values, na.rm = na.rm),
      max(values, na.rm = na.rm),
      CI,
      QU
    )
    names(results) <- c("min", "mean", "max", "lower", "upper", quant)
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
