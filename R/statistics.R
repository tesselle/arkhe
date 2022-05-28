# STATISTICS
#' @include AllClasses.R AllGenerics.R
NULL

# Bootstrap ====================================================================
#' @export
#' @rdname resample
#' @aliases resample,numeric-method
setMethod(
  f = "resample",
  signature = c(object = "numeric"),
  definition = function(object, do, n, ..., f = NULL) {
    size <- sum(object)
    replicates <- stats::rmultinom(n, size = size, prob = object / size)
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    if (is.function(f)) values <- f(values)
    values
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
  type <- match.arg(type, several.ok = FALSE)

  n <- length(x)
  z <- zscore(alpha = 1 - level, n = n, type = type)
  stardard_error <- stats::sd(x) / sqrt(n)

  margin <- z * stardard_error
  mean(x) + margin * c(-1, 1)
}

zscore <- function(alpha, n, type = c("student", "normal")) {
  switch(
    type,
    normal = stats::qnorm(1 - alpha / 2), # Large sample size
    student = stats::qt(1 - alpha / 2, n - 1), # Small sample size
    stop(sprintf("There is no such type: %s", type), call. = FALSE)
  )
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

  results <- c(jack_mean, jack_bias, jack_error)
  names(results) <- c("mean", "bias", "error")
  results
}
