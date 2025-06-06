# Assign row names =============================================================
df <- data.frame(
  a = 1:5,
  b = LETTERS[1:5]
)

df1 <- assign_rownames(df, column = 2)
df2 <- data.frame(
  a = 1:5,
  row.names = LETTERS[1:5]
)
expect_equal(df1, df2)

# Assign column names ==========================================================
df <- data.frame(
  a = LETTERS[1:5],
  b = LETTERS[6:10]
)

df1 <- assign_colnames(df, row = 2)
df2 <- data.frame(
  B = LETTERS[c(1, 3:5)],
  G = LETTERS[c(6, 8:10)],
  row.names = as.integer(c(1, 3:5))
)
expect_equal(df1, df2)
