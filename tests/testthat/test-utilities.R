context("Utilities")

# Test helpers
test_that("Compact", {
  expect_length(compact(is.null, list("A", NULL, "B")), 2)
})
test_that("Detect", {
  k <- detect(is.na, c(1, 2, NA, 4, 5, NA))
  expect_type(k, "logical")
  expect_equal(sum(k), 2)
})
test_that("Count", {
  k <- count(is.na, c(1, 2, NA, 4, 5, NA))
  expect_type(k, "integer")
  expect_equal(k, 2)
})
test_that("Extract", {
  k <- extract(c("abc123", "def456", "ghi"), "[1-9]{3}")
  expect_type(k, "character")
  expect_length(k, 2)
})
test_that("Function composition", {
  expect_type((sum %o% range)(1:5), "integer")
  expect_equal((sum %o% range)(1:5), 6)
})
test_that("NULL OR operator", {
  expect_type(NULL %||% 1, "double")
  expect_equal(NULL %||% 1, 1)
  expect_equal(0 %||% 1, 0)
})
test_that("Row and column index", {
  mtx <- matrix(data = 1:6, nrow = 2)

  expect_true(all(index_by_row(c(2, 3)) == row(mtx)))
  expect_error(index_by_row(1))
  expect_error(index_by_row("X"))

  expect_true(all(index_by_column(c(2, 3)) == col(mtx)))
  expect_error(index_by_column(1))
  expect_error(index_by_column("X"))
})
test_that("UUID", {
  id <- generate_uuid()
  expect_true(is_uuid(id))
})
