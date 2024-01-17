# Whitespace ===================================================================
x <- data.frame(A = c("  Both ", "  Left", "Right  ", "\n\nLeft\n  Middle Right\t"), B = 1:4)
xb <- xl <- xr <- x
xb$A <- trimws(x$A, which = "both")
xl$A <- trimws(x$A, which = "left")
xr$A <- trimws(x$A, which = "right")

expect_equal(clean_whitespace(x, which = "both", squish = FALSE), xb)
expect_equal(clean_whitespace(x, which = "left", squish = FALSE), xl)
expect_equal(clean_whitespace(x, which = "right", squish = FALSE), xr)

xb$A <- trimws(gsub(pattern = "\\s+", replacement = " ", x = x$A), which = "both")
expect_equal(clean_whitespace(x, which = "both", squish = TRUE), xb)
expect_equal(clean_whitespace(as.matrix(x), which = "both", squish = TRUE), as.matrix(xb))
