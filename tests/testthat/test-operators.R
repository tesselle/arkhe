context("Operators")

test_that("Arith", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(mtx + mtx, cts + cts)
  expect_equivalent(mtx * 2, cts * 2)
})
test_that("Compare", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(mtx == mtx, cts == cts)
  expect_equivalent(mtx > 2, cts > 2)
})
test_that("Logic", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(mtx & mtx, cts & cts)
  expect_equivalent(mtx & 1, cts & 1)
  expect_equivalent(mtx | FALSE, cts | FALSE)
})
test_that("Math", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(sqrt(mtx), sqrt(cts))
  expect_equivalent(cumsum(mtx), cumsum(cts))
})
test_that("Math2", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_abundance(mtx)
  mtx <- as.matrix(cts)

  expect_equivalent(signif(mtx, digits = 1), signif(cts, digits = 1))
})
test_that("Summary", {
  mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  expect_equivalent(range(mtx), range(cts))
  expect_equivalent(all(mtx), all(cts))
})
