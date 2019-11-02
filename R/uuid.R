# UUID

#' UUID
#'
#' \code{generate_uuid} generates a universally unique identifier (UUID Version
#' 4 and Variant 1).
#' @param x A \code{\link{character}} string (UUID).
#' @param seed A single \code{\link{integer}} specifying the seeds.
#'  If \code{NULL} (the default) the seed will be re-initialized.
#' @details
#'  As it relies on R's internal random number generators and so will suffer
#'  from the use of \code{\link{set.seed}} in a session, the seed is
#'  re-initialized during execution (unless \code{seed} is not \code{NULL}).
#'  To prevent any side effects, the random number generator (RNG) state is
#'  saved and restored when the function exits.
#' @return A 36 characters long \code{\link{character}} string.
#' @seealso \link{set.seed}
#' @author N. Frerebeau
#' @keywords internal
generate_uuid <- function(seed = NULL) {
  # Save and restore the random number generator (RNG) state
  if (!exists(".Random.seed", mode = "numeric")) sample(NA)
  old_seed <- .Random.seed
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  # Set seed
  seed <- if (is.numeric(seed)) seed else NULL
  set.seed(seed = seed)

  # Generate 32 pseudo random hex digits
  hex_digits <- c(as.character(0:9), letters[seq_len(6)])
  hex_32 <- sample(hex_digits, size = 32, replace = TRUE)
  # Set version (4) and variant (1)
  hex_32[13] <- 4
  hex_32[17] <- sample(c(8, 9, "a", "b"), size = 1)

  uuid <- paste(
    mapply(
      FUN = substr,
      start = c(1, 9, 13, 17, 21),
      stop = c(8, 12, 16, 20, 32),
      MoreArgs = list(x = paste0(hex_32, collapse = "")),
      SIMPLIFY = FALSE
    ),
    collapse = "-"
  )
  uuid
}
