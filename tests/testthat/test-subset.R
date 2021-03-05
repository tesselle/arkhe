test_that("Extract/replace with a numeric index", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_identical(cts[], cts)
  expect_identical(cts[, ], cts)

  expect_true(all(cts[1:2] == mtx[1:2]))
  expect_true(all(cts[1:2, ] == mtx[1:2, ]))
  expect_true(all(cts[, 3:4] == mtx[, 3:4]))
  expect_true(all(cts[1:2, 3] == mtx[1:2, 3]))
  expect_true(all(cts[1, , drop = TRUE] == mtx[1, , drop = TRUE]))
  expect_true(all(cts[, 1, drop = FALSE] == mtx[, 1, drop = FALSE]))

  cts[1] <- 1L
  expect_equal(cts[1], 1)
  expect_s4_class(cts, "CountMatrix")

  cts[1, 5] <- 0L
  expect_equal(cts[1, 5], 0)
  expect_s4_class(cts, "CountMatrix")

  cts[1, ] <- 0L
  expect_equal(cts[1, ], rep(0, 10), ignore_attr = TRUE)
  expect_s4_class(cts, "CountMatrix")

  expect_null(dim(cts[1, , drop = TRUE]))
  expect_equal(dim(cts[1, , drop = FALSE]), c(1, 10))

  cts[, 1] <- 1L
  expect_equal(cts[, 1], rep(1, 10), ignore_attr = TRUE)
  expect_s4_class(cts, "CountMatrix")

  expect_null(dim(cts[, 1, drop = TRUE]))
  expect_equal(dim(cts[, 1, drop = FALSE]), c(10, 1))

  cts[[1]] <- 0L
  expect_equal(cts[[1]], 0)
  expect_s4_class(cts, "CountMatrix")

  cts[[1, 5]] <- 999L
  expect_equal(cts[[1, 5]], 999)
  expect_s4_class(cts, "CountMatrix")

  expect_error(cts[[]])
  expect_error(cts[[1:2]])
  expect_error(cts[[, ]])
  expect_error(cts[[1, ]])
  expect_error(cts[[1:2, ]])
  expect_error(cts[[, 1]])
  expect_error(cts[[, 1:2]])

  freq <- as_composition(cts)
  expect_equal(get_totals(freq[1:5, ]), get_totals(freq)[1:5])
  expect_equal(get_totals(freq[5:10, 5:10]), get_totals(freq)[5:10])

  cts[] <- rep(0L, 100)
  expect_equal(sum(cts), 0)
})
