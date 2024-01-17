# CLEAN

# Whitespace ===================================================================
#' @export
#' @rdname clean_whitespace
#' @aliases clean_whitespace,data.frame-method
setMethod(
  f = "clean_whitespace",
  signature = c(x = "data.frame"),
  definition = function(x, which = c("both", "left", "right"), squish = TRUE) {
    x[] <- lapply(
      X = x,
      FUN = function(x, which, squish) {
        if (!is.character(x)) return(x)
        trim(x, which = which, squish = squish)
      },
      which = which,
      squish = squish
    )
    x
  }
)

#' @export
#' @rdname clean_whitespace
#' @aliases clean_whitespace,matrix-method
setMethod(
  f = "clean_whitespace",
  signature = c(x = "matrix"),
  definition = function(x, which = c("both", "left", "right"), squish = TRUE) {
    x[] <- trim(x, which = which, squish = squish)
    x
  }
)

trim <- function(x, which = c("both", "left", "right"), squish = TRUE) {
  ## Squish
  if (squish) x <- gsub(pattern = "\\s+", replacement = " ", x = x)
  ## Trim
  x <- trimws(x, which = which, whitespace = "[ \t\r\n]")

  x
}
