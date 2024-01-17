x <- data.frame(
  A = c("  Both ", "  Left", "Right  "),
  B = 1:3
)

clean_whitespace(x, which = "both")
clean_whitespace(x, which = "left")
clean_whitespace(x, which = "right")
