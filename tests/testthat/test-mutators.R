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
  cts <- CountMatrix(sample(1:100, 50, TRUE), ncol = 10)
  row_names <- paste0("row", 1:5)
  col_names <- paste0("col", 1:10)

  expect_equal(rownames(cts), row_names)
  expect_equal(colnames(cts), col_names)
  expect_equal(dimnames(cts), list(row_names, col_names))

  dimnames(cts) <- list(c("A", "B", "C", "D", "E"), 1:10)
  expect_equal(rownames(cts), c("A", "B", "C", "D", "E"))
  expect_equal(colnames(cts), as.character(1:10))


  cts <- CountMatrix(sample(1:100, 50, TRUE), ncol = 10,
                     dimnames = list(c("A", "A", "C", "D", "E"), 1:10))
  expect_equal(rownames(cts), c("A", "A_1", "C", "D", "E"))
  expect_equal(colnames(cts), as.character(1:10))
})
test_that("Matrix diagonal", {
  mtx <- matrix(sample(1:100, 50, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equal(diag(mtx), diag(cts))
})
