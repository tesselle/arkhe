# Seek -------------------------------------------------------------------------
expect_null(seek_columns(iris, select = startsWith, prefix = "sepal"))
expect_identical(seek_columns(iris, select = NULL), 1:5)
expect_identical(seek_columns(iris, select = startsWith, prefix = "Sepal"), c(1L, 2L))
expect_identical(seek_columns(iris, select = endsWith, suffix = "Width"), c(2L, 4L))
expect_identical(seek_columns(iris, names = c("Petal.Length", "Petal.Width")), c(3L, 4L))
expect_null(seek_columns(iris, names = c("XXX", "YYY")))
expect_error(seek_columns(iris, names = c(1, 2)), "must be character")

x <- data.frame(
  A = LETTERS,
  B = 1:26
)
expect_null(seek_rows(x, select = startsWith, prefix = "BDX"))
rownames(x) <- sprintf("BDX%04d", 1:26)
expect_identical(seek_rows(x, select = NULL), 1:26)
expect_identical(seek_rows(x, select = startsWith, prefix = "BDX"), 1:26)
expect_identical(seek_rows(x, select = endsWith, suffix = "6"), c(6L, 16L, 26L))
expect_identical(seek_rows(x, names = c("BDX0010", "BDX0020")), c(10L, 20L))
expect_null(seek_rows(x, names = c("XXX", "YYY")))
expect_error(seek_rows(x, names = c(1, 2)), "must be character")

# Get --------------------------------------------------------------------------
x <- as.matrix(x)
expect_identical(get_columns(x, select = startsWith, prefix = "A"), x[, 1, drop = FALSE])
expect_identical(get_rows(x, select = endsWith, suffix = "6"), x[c(6, 16, 26), , drop = FALSE])
