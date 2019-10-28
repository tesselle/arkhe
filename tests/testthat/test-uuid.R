context("UUID")

test_that("UUID", {
  id1 <- generate_uuid(seed = 12345)
  id2 <- generate_uuid(seed = 54321)

  expect_true(is_uuid(id1))
  expect_true(is_uuid(id2))
  expect_error(compare_uuid(id1, id2))
})
