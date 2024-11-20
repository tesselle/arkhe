mtx <- matrix(c(8L, 10L, 1L, 10L, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

## Nothing to remove
clean <- discard_rows(mtx, f = is_zero, all = FALSE)
expect_equal(dim(clean), dim(mtx))
expect_message(discard_rows(mtx, f = is_zero, verbose = TRUE))

mtx[1, ] <- 0 # Add zeros

## Nothing to remove
clean <- discard_columns(mtx, f = is_zero, all = TRUE)
expect_equal(dim(clean), dim(mtx))
expect_message(discard_columns(mtx, f = is_zero, all = TRUE, verbose = TRUE))

mtx[, 1] <- 0 # Add zeros

## Remove rows
clean_row <- discard_rows(mtx, f = is_zero, all = TRUE)
expect_equal(nrow(clean_row), nrow(mtx) - 1L)
expect_equal(ncol(clean_row), ncol(mtx))
expect_message(discard_rows(mtx, f = is_zero, all = TRUE, verbose = TRUE))

## Remove columns
clean_col <- discard_columns(mtx, f = is_zero, all = TRUE)
expect_equal(ncol(clean_col), ncol(mtx) - 1L)
expect_equal(nrow(clean_col), nrow(mtx))
expect_message(discard_columns(mtx, f = is_zero, all = TRUE, verbose = TRUE))
