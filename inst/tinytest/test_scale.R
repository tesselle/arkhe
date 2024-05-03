# Rescale continuous vector ====================================================
expect_identical(scale_range(5:10), c(0, 0.2, 0.4, 0.6, 0.8, 1))
expect_identical(scale_midpoint(5:10, to = c(0, 5)), c(3.75, 4, 4.25, 4.5, 4.75, 5))

# Label percentage =============================================================
expect_identical(label_percent(c(0.01, 0.5), trim = FALSE), c(" 1%", "50%"))
expect_identical(label_percent(c(0.01, 0.5), trim = TRUE), c("1%", "50%"))
