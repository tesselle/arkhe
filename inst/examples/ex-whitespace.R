x <- data.frame(
  A = c("  Both. ", "  Left.", "Right.  "),
  B = 1:3
)

remove_whitespace(x, which = "both")
remove_whitespace(x, which = "left")
remove_whitespace(x, which = "right")
