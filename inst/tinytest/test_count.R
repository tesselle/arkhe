# matrix =======================================================================
mtx <- matrix(c(8L, NA, 1L, NA, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, NA, 4L, 8L),
              nrow = 5, ncol = 3)

## Count missing values in rows
expect_equal(count(mtx, f = is.na, margin = 1), c(0, 1, 1, 1, 0))
## Count non-missing values in columns
no_na <- count(mtx, f = is.na, margin = 2, negate = TRUE)
expect_equal(no_na, c(V1 = 3, V2 = 5, V3 = 4))

# data.frame ===================================================================
df <- data.frame(
  V1 = c("A", NA, "B", NA, "C"),
  V2 = c(10L, 6L, 6L, 1L, 4L),
  V3 = c(2L, 1L, NA, 5L, 9L)
)

## Count missing values in rows
expect_equal(count(df, f = is.na, margin = 1), c(0, 1, 1, 1, 0))
## Count non-missing values in columns
no_na <- count(df, f = is.na, margin = 2, negate = TRUE)
expect_equal(no_na, c(V1 = 3, V2 = 5, V3 = 4))

## No margin
test <- df
test[] <- lapply(X = df, FUN = is.na)
expect_equal(count(df, f = is.na, margin = NULL), test)
