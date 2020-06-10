context("Data cleaning")

test_that("Missing values", {
  num <- sample(1:10, 25, TRUE)
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean <- remove_NA(mtx, margin = 1, finite = FALSE)
  expect_equal(dim(clean), dim(mtx))

  num[sample(1:25, 3, FALSE)] <- NA
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean_row <- remove_NA(mtx, margin = 1, finite = FALSE)
  expect_lt(nrow(clean_row), nrow(mtx))

  clean_col <- remove_NA(mtx, margin = 2, finite = FALSE)
  expect_lt(ncol(clean_col), ncol(mtx))

  options(arkhe.verbose = TRUE)
  expect_message(remove_NA(mtx, margin = 1, finite = FALSE))
})
test_that("Missing and non-finite values", {
  num <- sample(1:10, 25, TRUE)
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean <- remove_NA(mtx, margin = 1, finite = TRUE)
  expect_equal(dim(clean), dim(mtx))

  num[sample(1:25, 3, FALSE)] <- Inf
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  clean_row <- remove_NA(mtx, margin = 1, finite = TRUE)
  expect_lt(nrow(clean_row), nrow(mtx))

  clean_col <- remove_NA(mtx, margin = 2, finite = TRUE)
  expect_lt(ncol(clean_col), ncol(mtx))

  options(arkhe.verbose = TRUE)
  expect_message(remove_NA(mtx, margin = 1, finite = TRUE))
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
