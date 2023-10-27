# matrix =======================================================================
mtx <- matrix(c(8L, NA, 1L, NA, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, NA, 4L, 8L),
              nrow = 5, ncol = 3)

## Find row with NA
expect_equal(detect(mtx, f = is.na, margin = 1), c(FALSE, TRUE, TRUE, TRUE, FALSE))

## Find column without any NA
no_na <- detect(mtx, f = is.na, margin = 2, negate = TRUE, all = TRUE)
expect_equal(no_na, c(FALSE, TRUE, FALSE))

# data.frame ===================================================================
df <- data.frame(
  V1 = c("A", NA, "B", NA, "C"),
  V2 = c(10L, 6L, 6L, 1L, 4L),
  V3 = c(2L, 1L, NA, 5L, 9L)
)

## Find row with NA
expect_equal(detect(df, f = is.na, margin = 1), c(FALSE, TRUE, TRUE, TRUE, FALSE))

## Find column without any NA
no_na <- detect(df, f = is.na, margin = 2, negate = TRUE, all = TRUE)
expect_equal(no_na, c(V1 = FALSE, V2 = TRUE, V3 = FALSE))

## Find numeric column
expect_equal(detect(df, f = is.numeric, margin = 2), c(V1 = FALSE, V2 = TRUE, V3 = TRUE))
