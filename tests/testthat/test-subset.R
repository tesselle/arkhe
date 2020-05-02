context("Subset")

test_that("Extract/replace with a numeric index", {
  mtx <- CountMatrix(data = sample(2:10, 100, TRUE), ncol = 10)

  expect_equivalent(mtx[], as(mtx, "matrix"))
  expect_equivalent(mtx[, ], as(mtx, "matrix"))

  mtx[1] <- 1
  expect_equivalent(mtx[1], 1)
  expect_s4_class(mtx, "CountMatrix")

  mtx[1, 5] <- 0
  expect_equivalent(mtx[1, 5], 0)
  expect_s4_class(mtx, "CountMatrix")

  mtx[1, ] <- 0
  expect_equivalent(mtx[1, ], rep(0, 10))
  expect_s4_class(mtx, "CountMatrix")

  expect_null(dim(mtx[1, , drop = TRUE]))
  expect_equal(dim(mtx[1, , drop = FALSE]), c(1, 10))

  mtx[, 1] <- 1
  expect_equivalent(mtx[, 1], rep(1, 10))
  expect_s4_class(mtx, "CountMatrix")

  expect_null(dim(mtx[, 1, drop = TRUE]))
  expect_equal(dim(mtx[, 1, drop = FALSE]), c(10, 1))

  mtx[[1]] <- 0
  expect_equivalent(mtx[[1]], 0)
  expect_s4_class(mtx, "CountMatrix")

  mtx[[1, 5]] <- 999
  expect_equivalent(mtx[[1, 5]], 999)
  expect_s4_class(mtx, "CountMatrix")

  expect_error(mtx[[]])
  expect_error(mtx[[, ]])
  # expect_error(mtx[[1, ]]) # TODO: investigate
  expect_error(mtx[[, 1]])
})
