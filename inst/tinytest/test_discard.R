mtx <- matrix(c(8L, 10L, 1L, 10L, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

## Nothing to remove
clean <- discard_rows(mtx, f = is_zero, all = FALSE)
expect_equal(dim(clean), dim(mtx))
expect_message(discard_rows(mtx, f = is_zero, verbose = TRUE),
               "No rows to remove")

mtx[1, ] <- 0 # Add zeros

## Nothing to remove
clean <- discard_cols(mtx, f = is_zero, all = TRUE)
expect_equal(dim(clean), dim(mtx))
expect_message(discard_cols(mtx, f = is_zero, all = TRUE, verbose = TRUE),
               "No columns to remove")

mtx[, 1] <- 0 # Add zeros

## Remove rows
clean_row <- discard_rows(mtx, f = is_zero, all = TRUE)
expect_equal(nrow(clean_row), nrow(mtx) - 1L)
expect_equal(ncol(clean_row), ncol(mtx))
expect_message(discard_rows(mtx, f = is_zero, all = TRUE, verbose = TRUE),
               "Removing 1 row out of 5")

## Remove columns
clean_col <- discard_cols(mtx, f = is_zero, all = TRUE)
expect_equal(ncol(clean_col), ncol(mtx) - 1L)
expect_equal(nrow(clean_col), nrow(mtx))
expect_message(discard_cols(mtx, f = is_zero, all = TRUE, verbose = TRUE),
               "Removing 1 column out of 3")

## No margin
test <- apply(X = mtx, MARGIN = 2, FUN = is_zero)
expect_equivalent(discard(mtx, f = is_zero, margin = NULL), test)
