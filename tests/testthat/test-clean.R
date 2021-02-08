test_that("Replace missing values", {
  options(arkhe.verbose = FALSE)
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  mtx[sample(1:25, 3, FALSE)] <- NA # Add missing values

  # Replace NAs
  clean <- replace_NA(mtx, value = 999)
  expect_equal(sum(clean == 999), 3)

  ct <- as_count(mtx) # Create CountMatrix

  # Replace NAs
  ct_clean <- replace_NA(ct, value = 999L)
  expect_equal(sum(ct_clean == 999), 3)
})
test_that("Remove missing values", {
  options(arkhe.verbose = FALSE)
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_NA(mtx, margin = 1, finite = FALSE)
  expect_equal(dim(clean), dim(mtx))

  mtx[sample(1:25, 3, FALSE)] <- NA # Add missing values

  # Remove rows
  clean_row <- remove_NA(mtx, margin = 1, finite = FALSE)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_row <- remove_NA(mtx, margin = 2, finite = FALSE)
  expect_lt(ncol(clean_row), ncol(mtx))
  expect_equal(nrow(clean_row), nrow(mtx))

  ct <- as_count(mtx) # Create CountMatrix

  # Remove rows
  ct_clean_row <- remove_NA(ct, margin = 1, finite = FALSE)
  expect_lt(nrow(ct_clean_row), nrow(ct))
  expect_equal(ncol(ct_clean_row), ncol(ct))

  # Remove columns
  ct_clean_col <- remove_NA(ct, margin = 2, finite = FALSE)
  expect_lt(ncol(ct_clean_col), ncol(ct))
  expect_equal(nrow(ct_clean_col), nrow(ct))

  # Check message
  options(arkhe.verbose = TRUE)
  expect_message(remove_NA(mtx, margin = 1, finite = FALSE))
})
test_that("Remove missing and non-finite values", {
  options(arkhe.verbose = FALSE)
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_NA(mtx, margin = 1, finite = TRUE)
  expect_equal(dim(clean), dim(mtx))

  mtx[sample(1:25, 3, FALSE)] <- Inf # Add infinite values

  # Nothing to remove
  clean <- remove_NA(mtx, margin = 1, finite = FALSE)
  expect_equal(dim(clean), dim(mtx))

  # Remove rows
  clean_row <- remove_NA(mtx, margin = 1, finite = TRUE)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_row <- remove_NA(mtx, margin = 2, finite = TRUE)
  expect_lt(ncol(clean_row), ncol(mtx))
  expect_equal(nrow(clean_row), nrow(mtx))

  # Check message
  options(arkhe.verbose = TRUE)
  expect_message(remove_NA(mtx, margin = 1, finite = TRUE))
})
test_that("Remove zeros", {
  options(arkhe.verbose = FALSE)
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_NA(mtx, margin = 1, finite = TRUE)
  expect_equal(dim(clean), dim(mtx))

  mtx[sample(1:25, 3, FALSE)] <- 0 # Add zeros

  # Remove rows
  clean_row <- remove_zero(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_row <- remove_zero(mtx, margin = 2)
  expect_lt(ncol(clean_row), ncol(mtx))
  expect_equal(nrow(clean_row), nrow(mtx))

  ct <- as_count(mtx) # Create CountMatrix

  # Remove rows
  ct_clean_row <- remove_zero(ct, margin = 1)
  expect_lt(nrow(ct_clean_row), nrow(ct))
  expect_equal(ncol(ct_clean_row), ncol(ct))

  # Remove columns
  ct_clean_col <- remove_zero(ct, margin = 2)
  expect_lt(ncol(ct_clean_col), ncol(ct))
  expect_equal(nrow(ct_clean_col), nrow(ct))

  # Check message
  options(arkhe.verbose = TRUE)
  expect_message(remove_zero(mtx, margin = 1))
})
test_that("Remove empty row/column", {
  options(arkhe.verbose = FALSE)
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_empty(mtx, margin = 1)
  expect_equal(dim(clean), dim(mtx))

  mtx[1, ] <- 0 # Add zeros
  mtx[, 1] <- 0 # Add zeros

  # Remove rows
  clean_row <- remove_empty(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- remove_empty(mtx, margin = 2)
  expect_lt(ncol(clean_col), ncol(mtx))
  expect_equal(nrow(clean_col), nrow(mtx))

  ct <- as_count(mtx) # Create CountMatrix

  # Remove rows
  ct_clean_row <- remove_empty(ct, margin = 1)
  expect_lt(nrow(ct_clean_row), nrow(ct))
  expect_equal(ncol(ct_clean_row), ncol(ct))

  # Remove columns
  ct_clean_col <- remove_empty(ct, margin = 2)
  expect_lt(ncol(ct_clean_col), ncol(ct))
  expect_equal(nrow(ct_clean_col), nrow(ct))

  # Check message
  options(arkhe.verbose = TRUE)
  expect_message(remove_empty(mtx, margin = 1))
})
