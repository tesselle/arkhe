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
test_that("UUID", {
  id1 <- generate_uuid(seed = 12345)
  id2 <- generate_uuid(seed = 54321)

  expect_type(id1, "character")
  expect_type(id2, "character")

  expect_equal(nchar(id1), 36)
  expect_equal(nchar(id2), 36)

  expect_true(is_uuid(id1))
  expect_true(is_uuid(id2))

  expect_error(check_uuid(character(0)))
  expect_error(check_uuid("X"))
  expect_warning(check_uuid("00000000-0000-4000-a000-000000000000"))
})
