context("Mutators")

test_that("Matrix dimensions", {
  cts <- CountMatrix(sample(1:100, 50, TRUE), ncol = 10)

  expect_equal(length(cts), 50)
  expect_equal(dim(cts), c(5, 10))
  expect_equal(nrow(cts), 5)
  expect_equal(ncol(cts), 10)

  expect_equal(row(cts), matrix(rep(1:5, times = 10), ncol = 10))
  expect_equal(col(cts), matrix(rep(1:10, each = 5), ncol = 10))

  expect_type(row(cts, as.factor = TRUE), "integer")
  expect_type(col(cts, as.factor = TRUE), "integer")
})
test_that("Matrix dimension names", {
  cts <- CountMatrix(nrow = 0, ncol = 0)
  expect_null(rownames(cts))
  expect_null(colnames(cts))

  cts <- CountMatrix(sample(1:100, 50, TRUE), ncol = 10)
  row_names <- paste0("row", 1:5)
  col_names <- paste0("col", 1:10)

  expect_equal(rownames(cts), row_names)
  expect_equal(colnames(cts), col_names)
  expect_equal(dimnames(cts), list(row_names, col_names))

  dim_names <- list(c("A", "B", "C", "D", "E"), as.character(1:10))
  dimnames(cts) <- dim_names
  expect_equal(rownames(cts), dim_names[[1L]])
  expect_equal(colnames(cts), dim_names[[2L]])

  rownames(cts) <- c("A", "A", "C", "D", "E")
  expect_equal(rownames(cts), c("A", "A_1", "C", "D", "E"))

  colnames(cts) <- c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E")
  expect_equal(colnames(cts), c("A", "B", "C", "D", "E", "A_1", "B_1", "C_1", "D_1", "E_1"))
})
test_that("Matrix diagonal", {
  mtx <- matrix(sample(1:100, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equal(diag(cts), diag(mtx))

  diag(cts) <- rep(0, times = 10)
  expect_equal(diag(cts), rep(0, times = 10))
})
