mtx <- matrix(c(8L, 10L, 1L, 10L, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

# Nothing to keep
clean <- keep_rows(mtx, f = is_zero, all = FALSE)
expect_equal(dim(clean), c(0, 3))

mtx[1, ] <- 0 # Add zeros

# Nothing to keep
clean <- keep_columns(mtx, f = is_zero, all = TRUE)
expect_equal(dim(clean), c(5, 0))

mtx[, 1] <- 0 # Add zeros

# Keep all
clean_row <- keep_rows(mtx, f = is_zero, all = FALSE)
expect_equal(clean_row, mtx)
expect_message(keep_rows(mtx, f = is_zero, verbose = TRUE))

clean_col <- keep_columns(mtx, f = is_zero, all = FALSE)
expect_equal(clean_col, mtx)
expect_message(keep_columns(mtx, f = is_zero, verbose = TRUE))

# Remove rows
clean_row <- keep_rows(mtx, f = is_zero, all = TRUE)
expect_equal(dim(clean_row), c(1, 3))
expect_message(keep_rows(mtx, f = is_zero, all = TRUE, verbose = TRUE))

# Remove columns
clean_col <- keep_columns(mtx, f = is_zero, all = TRUE)
expect_equal(dim(clean_col), c(5, 1))
expect_message(keep_columns(mtx, f = is_zero, all = TRUE, verbose = TRUE))
