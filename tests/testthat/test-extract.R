context("Extract")

test_that("Matrix", {
  mtx <- .Matrix()
  expect_type(get_id(mtx), "character")

  mtx <- .Matrix(sample(1:100, 100, TRUE), ncol = 10)
  mtx[[1]] <- 0
  expect_equal(mtx[[1]], 0)

  mtx[[1, 1]] <- 1
  expect_equal(mtx[[1, 1]], 1)

  mtx[1] <- 0
  expect_equal(mtx[1], 0)

  mtx[1, ] <- 0
  expect_equal(mtx[1, ], rep(0, 10))

  mtx[, 1] <- 0
  expect_equal(mtx[, 1], rep(0, 10))
})
test_that("NumericMatrix", {
  count <- matrix(sample(1:100, 100, TRUE), ncol = 10)

  freq <- .AbundanceMatrix(count / rowSums(count), totals = rowSums(count))
  expect_equal(get_totals(freq), rowSums(count))

  set_totals(freq) <- seq_len(10)
  expect_equal(get_totals(freq), seq_len(10))
})
test_that("SimilarityMatrix", {
  sim <- .SimilarityMatrix()
  expect_equal(get_method(sim), "unknown")
})
