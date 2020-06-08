context("Data cleaning")

test_that("Missing values", {
  num <- sample(1:10, 25, TRUE)
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean <- remove_missing(mtx, margin = 1)
  expect_equal(dim(clean), dim(mtx))

  num[sample(1:25, 3, FALSE)] <- NA
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean_row <- remove_missing(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))

  clean_col <- remove_missing(mtx, margin = 2)
  expect_lt(ncol(clean_col), ncol(mtx))

  options(arkhe.verbose = TRUE)
  expect_message(remove_missing(mtx, margin = 1))
})
test_that("Zeros", {
  num <- sample(1:10, 25, TRUE)
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean <- remove_zero(mtx, margin = 1)
  expect_equal(dim(clean), dim(mtx))

  num[sample(1:25, 3, FALSE)] <- 0
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean_row <- remove_zero(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))

  clean_col <- remove_zero(mtx, margin = 2)
  expect_lt(ncol(clean_col), ncol(mtx))

  options(arkhe.verbose = TRUE)
  expect_message(remove_zero(mtx, margin = 1))
})
