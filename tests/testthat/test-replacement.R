context("Replace")

test_that("Matrix", {
  mtx <- .Matrix(sample(2:100, 100, TRUE), ncol = 10)

  mtx[[1]] <- 0
  expect_equal(mtx[[1]], 0)
  expect_s4_class(mtx, "Matrix")

  mtx[[3, 3]] <- 1
  expect_equal(mtx[[3, 3]], 1)
  expect_s4_class(mtx, "Matrix")

  mtx[1] <- 0
  expect_equal(mtx[1], 0)
  expect_s4_class(mtx, "Matrix")

  mtx[1, ] <- 1
  expect_equal(mtx[1, ], rep(1, 10))
  expect_s4_class(mtx, "Matrix")

  mtx[, 1] <- 0
  expect_equal(mtx[, 1], rep(0, 10))
  expect_s4_class(mtx, "Matrix")
})
test_that("CountMatrix", {
  mtx <- CountMatrix(data = sample(2:10, 100, TRUE), ncol = 10)

  mtx[[1]] <- 0
  expect_equal(mtx[[1]], 0)
  expect_s4_class(mtx, "CountMatrix")

  mtx[[3, 3]] <- 1
  expect_equal(mtx[[3, 3]], 1)
  expect_s4_class(mtx, "Matrix")

  mtx[1] <- 0
  expect_equal(mtx[1], 0)
  expect_s4_class(mtx, "CountMatrix")

  mtx[1, ] <- 1
  expect_equivalent(mtx[1, ], rep(1, 10))
  expect_s4_class(mtx, "CountMatrix")

  mtx[, 1] <- 0
  expect_equivalent(mtx[, 1], rep(0, 10))
  expect_s4_class(mtx, "CountMatrix")

  expect_error(mtx[1, 1] <- -1)
  expect_error(mtx[1, 1] <- 1.1)
  expect_error(mtx[1, 1] <- "X")
})
