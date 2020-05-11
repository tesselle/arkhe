context("Statistics")

test_that("Means", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(rowMeans(mtx), rowMeans(cts))
  expect_equivalent(colMeans(mtx), colMeans(cts))
})
test_that("Sums", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(rowSums(mtx), rowSums(cts))
  expect_equivalent(colSums(mtx), colSums(cts))
})
test_that("Apply", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(rowSums(mtx), apply(X = cts, MARGIN = 1, FUN = sum))
  expect_equivalent(colSums(mtx), apply(X = cts, MARGIN = 2, FUN = sum))
})
