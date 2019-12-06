context("Geography")

test_that("coordinates can be set with a matrix or data.frame", {
  X <- matrix(data = sample(1:10, 30, TRUE), nrow = 10)
  expect_type(make_coordinates(X), "integer")

  X <- matrix(data = sample(1:10, 30, TRUE), nrow = 10,
              dimnames = list(NULL, c("X", "Y", "Z")))
  expect_type(make_coordinates(X), "integer")

  X <- matrix(data = sample(1:10, 10, TRUE), nrow = 10)
  expect_error(make_coordinates(X), "must have at least 2 columns")
})
test_that("coordinates can be set with a list", {
  X <- list(X = seq_len(10), Y = rep(0, 10))
  expect_type(make_coordinates(X), "double")

  X <- list(X = seq_len(10))
  expect_error(make_coordinates(X), "does not have components")
})
