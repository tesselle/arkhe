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
