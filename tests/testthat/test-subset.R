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
  expect_identical(get_totals(freq[1:5, ]), get_totals(freq)[1:5])
  expect_identical(get_totals(freq[5:10, 5:10]), get_totals(freq)[5:10])

  cts[] <- rep(0L, 100)
  expect_equal(sum(cts), 0)
})
test_that("Subset extra slots", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 5)
  cts <- as_count(mtx)

  set_groups(cts) <- rep(c("A", "B"), each = 10)
  set_samples(cts) <- rep(c("X", "Y"), times = 10)
  set_dates(cts) <- 1:20
  set_tpq(cts) <- 1:20
  set_taq(cts) <- 21:40

  tmp <- cts[1:10, ]
  expect_identical(get_groups(tmp), rep("A", 10))
  expect_identical(get_samples(tmp), rep(c("X", "Y"), times = 5))
  expect_identical(get_dates(tmp), 1:10)
  expect_identical(get_tpq(tmp), 1:10)
  expect_identical(get_taq(tmp), 21:30)
})
