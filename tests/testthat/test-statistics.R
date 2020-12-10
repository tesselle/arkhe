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
test_that("Variance-Covariance", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(var(cts), var(mtx))
  expect_equivalent(cov(cts), cov(mtx))
  expect_equivalent(cor(cts), cor(mtx))
})
test_that("Summary", {
  cts <- CountMatrix(sample(1:100, 75, TRUE), ncol = 5)
  set_groups(cts) <- rep(c("A", "B", "C"), each = 5)

  res <- summary(cts)
  expect_s4_class(res, "MatrixSummary")
  expect_length(res, 3)
  for (i in length(res)) {
    expect_s3_class(res[[i]]$stats, "table")
  }

  expect_output(show(res), "--- A")
})
