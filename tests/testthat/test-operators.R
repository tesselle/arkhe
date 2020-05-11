context("Operators")

test_that("Arith", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_identical(mtx + mtx, cts + cts)
  expect_identical(mtx * 2, cts * 2)
})
test_that("Compare", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_identical(mtx == mtx, cts == cts)
  expect_identical(mtx > 2, cts > 2)
})
test_that("Logic", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_identical(mtx & mtx, cts & cts)
  expect_identical(mtx & 1, cts & 1)
  expect_identical(mtx | FALSE, cts | FALSE)
})
test_that("Summary", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_identical(range(mtx), range(cts))
  expect_identical(all(mtx), all(cts))
})
