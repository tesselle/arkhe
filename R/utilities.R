# HELPERS

## https://stackoverflow.com/questions/56191862/where-do-i-specify-random-seed-for-tests-in-r-package
#' Evaluate an Expression with a Temporarily Seed
#'
#' @param expr An [`expression`] to be evaluated.
#' @param seed A single value to be passed to [set.seed()].
#' @param envir	The [environment][environment()] in which `expr` should be
#'  evaluated.
#' @param rounding A [`logical`] scalar: should the default discrete uniform
#'  generation method in \R versions prior to 3.6.0 be used? Usefull for unit
#'  testing.
#' @param ... Further arguments to be passed to [set.seed()].
#' @return
#'  The results of `expr` evaluated.
#' @seealso [set.seed()]
#' @keywords internal
with_seed <- function(expr, seed, ..., envir = parent.frame(), rounding = TRUE) {
  expr <- substitute(expr)
  ## Save and restore the random number generator (RNG) state
  env <- globalenv()
  old_seed <- env$.Random.seed
  on.exit({
    if (is.null(old_seed)) {
      rm(list = ".Random.seed", envir = env, inherits = FALSE)
    } else {
      assign(".Random.seed", value = old_seed, envir = env, inherits = FALSE)
    }
  })
  ## Keep the results the same for R versions prior to 3.6
  if (isTRUE(rounding) && getRversion() >= "3.6") {
    ## Set sample.kind = "Rounding" to reproduce the old sampling
    ## Suppress warning "non-uniform 'Rounding' sampler used"
    suppressWarnings(set.seed(seed, sample.kind = "Rounding"))
  } else {
    set.seed(seed)
  }
  eval(expr, envir = envir)
}

# Helpers ======================================================================
#' Helpers
#'
# * `compact()` removes elements from a list or vector.
# * `detect()` finds values in a list or vector according to a given predicate.
# * `count()` counts values in a list or vector according to a given predicate.
#' * `extract()` extracts a string form another string based on a pattern.
#' * `\%o\%` allows for function composition.
#' * `\%||\%` allows to define a default value.
#' @param x,y An object.
#' @param f,g A [`function`]. In `compact()`, `detect()` and `count()` `f` must
#'  be a [`logical`] predicate.
#' @param pattern A [`character`] string containing a regular expression.
#' @references
#'  Wickham, H. (2014). *Advanced R*. London: Chapman & Hall. The R Series.
#' @family utilities
#' @keywords internal utilities
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}
`%o%` <- function(f, g) {
  function(...) f(g(...))
}
# compact <- function(f, x) {
#   Filter(Negate(f), x)
# }
# detect <- function(f, x) {
#   vapply(x, f, logical(1))
# }
# count <- function(f, x) {
#   sum(detect(f, x))
# }
extract <- function(x, pattern) {
  regmatches(x, regexpr(pattern, x))
}
