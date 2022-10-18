test_that("Count", {
  num <- sample(1:10, 15, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 3)

  mtx[c(2, 4, 13)] <- NA # Add missing values

  ## Count missing values in rows
  expect_equal(count(mtx, f = is.na, margin = 1), c(0, 1, 1, 1, 0))
  ## Count non-missing values in columns
  no_na <- count(mtx, f = is.na, margin = 2, negate = TRUE)
  expect_equal(no_na, c(3, 5, 4))

  df <- as.data.frame(mtx)

  ## Count missing values in rows
  expect_equal(count(df, f = is.na, margin = 1), c(0, 1, 1, 1, 0))
  ## Count non-missing values in columns
  no_na <- count(df, f = is.na, margin = 2, negate = TRUE)
  expect_equal(no_na, c(V1 = 3, V2 = 5, V3 = 4))
})
test_that("Detect", {
  num <- sample(1:10, 15, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 3)

  mtx[c(2, 4, 13)] <- NA # Add missing values

  ## Find row with NA
  expect_equal(detect(mtx, f = is.na, margin = 1), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  ## Find column without any NA
  no_na <- detect(mtx, f = is.na, margin = 2, negate = TRUE, all = TRUE)
  expect_equal(no_na, c(FALSE, TRUE, FALSE))

  df <- as.data.frame(mtx)

  ## Find row with NA
  expect_equal(detect(df, f = is.na, margin = 1), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  ## Find column without any NA
  no_na <- detect(df, f = is.na, margin = 2, negate = TRUE, all = TRUE)
  expect_equal(no_na, c(V1 = FALSE, V2 = TRUE, V3 = FALSE))
})
test_that("Discard", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- discard_rows(mtx, f = is_zero, all = FALSE)
  expect_equal(dim(clean), dim(mtx))

  mtx[1, ] <- 0 # Add zeros

  # Nothing to remove
  clean <- discard_cols(mtx, f = is_zero, all = TRUE)
  expect_equal(dim(clean), dim(mtx))

  mtx[, 1] <- 0 # Add zeros

  # Remove rows
  clean_row <- discard_rows(mtx, f = is_zero, all = TRUE)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- discard_cols(mtx, f = is_zero, all = TRUE)
  expect_lt(ncol(clean_col), ncol(mtx))
  expect_equal(nrow(clean_col), nrow(mtx))
})
test_that("Keep", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to keep
  clean <- keep_rows(mtx, f = is_zero, all = FALSE)
  expect_equal(dim(clean), c(0, 5))

  mtx[1, ] <- 0 # Add zeros

  # Nothing to keep
  clean <- keep_cols(mtx, f = is_zero, all = TRUE)
  expect_equal(dim(clean), c(5, 0))

  mtx[, 1] <- 0 # Add zeros

  # Keep all
  clean_row <- keep_rows(mtx, f = is_zero, all = FALSE)
  expect_equal(dim(clean_row), dim(mtx))

  clean_col <- keep_cols(mtx, f = is_zero, all = FALSE)
  expect_equal(dim(clean_col), dim(mtx))

  # Remove rows
  clean_row <- keep_rows(mtx, f = is_zero, all = TRUE)
  expect_equal(dim(clean_row), c(1, 5))

  # Remove columns
  clean_col <- keep_cols(mtx, f = is_zero, all = TRUE)
  expect_equal(dim(clean_col), c(5, 1))
})
# Missing values ===============================================================
test_that("Replace missing values", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  mtx[sample(1:25, 3, FALSE)] <- NA # Add missing values

  # Replace NAs
  clean <- replace_NA(mtx, value = 999)
  expect_equal(sum(clean == 999), 3)
})
test_that("Remove missing values", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_NA(mtx, margin = 1)
  expect_equal(dim(clean), dim(mtx))

  mtx[sample(1:25, 3, FALSE)] <- NA # Add missing values

  # Remove rows
  clean_row <- remove_NA(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_gt(nrow(clean_row), 0)
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- remove_NA(mtx, margin = 2)
  expect_lt(ncol(clean_col), ncol(mtx))
  expect_gt(ncol(clean_col), 0)
  expect_equal(nrow(clean_col), nrow(mtx))
})
# Infinite values ==============================================================
test_that("Replace infinite values", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  mtx[sample(1:25, 3, FALSE)] <- Inf # Add infinite values

  # Replace Inf
  clean <- replace_Inf(mtx, value = 999)
  expect_equal(sum(clean == 999), 3)
})
test_that("Remove infinite values", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_Inf(mtx, margin = 1)
  expect_equal(dim(clean), dim(mtx))

  mtx[sample(1:25, 3, FALSE)] <- Inf # Add infinite values

  # Remove rows
  clean_row <- remove_Inf(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_gt(nrow(clean_row), 0)
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- remove_Inf(mtx, margin = 2)
  expect_lt(ncol(clean_col), ncol(mtx))
  expect_gt(ncol(clean_col), 0)
  expect_equal(nrow(clean_col), nrow(mtx))
})
# Zeros ========================================================================
test_that("Replace zeros", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  mtx[sample(1:25, 3, FALSE)] <- 0 # Add zeros

  # Replace 0s
  clean <- replace_zero(mtx, value = 999)
  expect_equal(sum(clean == 999), 3)
})
test_that("Remove zeros", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- remove_zero(mtx, margin = 1)
  expect_equal(dim(clean), dim(mtx))

  mtx[sample(1:25, 3, FALSE)] <- 0 # Add zeros

  # Remove rows
  clean_row <- remove_zero(mtx, margin = 1)
  expect_lt(nrow(clean_row), nrow(mtx))
  expect_gt(nrow(clean_row), 0)
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- remove_zero(mtx, margin = 2)
  expect_lt(ncol(clean_col), ncol(mtx))
  expect_gt(ncol(clean_col), 0)
  expect_equal(nrow(clean_col), nrow(mtx))
})
# Empty rows/columns ===========================================================
test_that("Compact (numeric)", {
  num <- sample(1:10, 25, TRUE) # Create matrix
  mtx <- matrix(data = num, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- compact_rows(mtx)
  expect_equal(dim(clean), dim(mtx))

  mtx[1, ] <- 0 # Add zeros
  mtx[, 1] <- 0 # Add zeros

  # Remove rows
  clean_row <- compact_rows(mtx)
  expect_equal(nrow(clean_row), 4)
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- compact_cols(mtx)
  expect_equal(ncol(clean_col), 4)
  expect_equal(nrow(clean_col), nrow(mtx))
})
test_that("Compact (character)", {
  char <- sample(LETTERS, 25, TRUE) # Create matrix
  mtx <- matrix(data = char, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- compact_rows(mtx)
  expect_equal(dim(clean), dim(mtx))

  mtx[1, ] <- "" # Add blank
  mtx[, 1] <- "" # Add blank

  # Remove rows
  clean_row <- compact_rows(mtx)
  expect_equal(nrow(clean_row), 4)
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- compact_cols(mtx)
  expect_equal(ncol(clean_col), 4)
  expect_equal(nrow(clean_col), nrow(mtx))
})
test_that("Compact (logical)", {
  char <- sample(c(TRUE, FALSE), 25, TRUE) # Create matrix
  mtx <- matrix(data = char, nrow = 5, ncol = 5)

  # Nothing to remove
  clean <- compact_rows(mtx)
  expect_equal(dim(clean), dim(mtx))

  mtx[1, ] <- NA # Add NA
  mtx[, 1] <- NA # Add NA

  # Remove rows
  clean_row <- compact_rows(mtx)
  expect_equal(nrow(clean_row), 4)
  expect_equal(ncol(clean_row), ncol(mtx))

  # Remove columns
  clean_col <- compact_cols(mtx)
  expect_equal(ncol(clean_col), 4)
  expect_equal(nrow(clean_col), nrow(mtx))
})
