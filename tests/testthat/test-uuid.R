context("UUID")

test_that("UUID", {
  id1 <- generate_uuid(seed = 12345)
  id2 <- generate_uuid(seed = 54321)

  expect_type(id1, "character")
  expect_type(id2, "character")

  expect_equal(nchar(id1), 36)
  expect_equal(nchar(id2), 36)

  expect_true(is_uuid(id1))
  expect_true(is_uuid(id2))
})
