# Append row names =============================================================
df <- data.frame(
  x = 1:5,
  y = 6:10,
  row.names = LETTERS[1:5]
)

df1 <- append_rownames(df, after = 3, remove = TRUE)
df2 <- data.frame(
  x = 1:5,
  y = 6:10,
  rownames = LETTERS[1:5]
)
expect_identical(df1, df2)

# Append column ================================================================
df <- data.frame(
  x = 1:5,
  y = 6:10,
  row.names = LETTERS[1:5]
)

col <- c(D = 44, E = 55, B = 22)
df1 <- append_column(df, col, after = 3, var = "new_col")
df2 <- data.frame(
  x = 1:5,
  y = 6:10,
  new_col = c(NA, 22, NA, 44, 55),
  row.names = LETTERS[1:5]
)
expect_identical(df1, df2)

expect_error(append_column(df, unname(col)))

col <- c(DD = 44, EE = 55, BB = 22)
df1 <- append_column(df, col, after = 3)
df2 <- data.frame(
  x = 1:5,
  y = 6:10,
  .col = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
  row.names = LETTERS[1:5]
)
expect_identical(df1, df2)
