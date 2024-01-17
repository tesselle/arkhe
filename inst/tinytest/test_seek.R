# Seek -------------------------------------------------------------------------
expect_null(seek_columns(iris, select = startsWith, prefix = "sepal"))
expect_identical(seek_columns(iris, select = NULL), 1:5)
expect_identical(seek_columns(iris, select = startsWith, prefix = "Sepal"), c(1L, 2L))
expect_identical(seek_columns(iris, select = endsWith, suffix = "Width"), c(2L, 4L))

x <- data.frame(
  A = LETTERS,
  B = 1:26
)
expect_null(seek_rows(x, select = startsWith, prefix = "BDX"))
rownames(x) <- sprintf("BDX%04d", 1:26)
expect_identical(seek_rows(x, select = NULL), 1:26)
expect_identical(seek_rows(x, select = startsWith, prefix = "BDX"), 1:26)
expect_identical(seek_rows(x, select = endsWith, suffix = "6"), c(6L, 16L, 26L))

# Get --------------------------------------------------------------------------
expect_identical(get_columns(x, select = startsWith, prefix = "A"), x[, 1, drop = FALSE])
expect_identical(get_rows(x, select = endsWith, suffix = "6"), x[c(6, 16, 26), , drop = FALSE])
