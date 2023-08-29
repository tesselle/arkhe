mtx <- matrix(c(8L, 10L, 1L, 10L, 2L,
                5L, 10L, 10L, 3L, 3L,
                6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

mtxNA <- matrix(c(8L, NA, 1L, NA, 2L,
                  5L, 10L, 10L, 3L, 3L,
                  6L, 8L, NA, 4L, 8L),
                nrow = 5, ncol = 3)

dfNA <- data.frame(
  V1 = c("A", NA, "B", NA, "C"),
  V2 = c(10L, 6L, 6L, 1L, 4L),
  V3 = c(2L, 1L, NA, 5L, 9L)
)

# Count ========================================================================
## Count missing values in rows
expect_equal(count(mtxNA, f = is.na, margin = 1), c(0, 1, 1, 1, 0))
## Count non-missing values in columns
no_na <- count(mtxNA, f = is.na, margin = 2, negate = TRUE)
expect_equal(no_na, c(V1 = 3, V2 = 5, V3 = 4))

## Count missing values in rows
expect_equal(count(dfNA, f = is.na, margin = 1), c(0, 1, 1, 1, 0))
## Count non-missing values in columns
no_na <- count(dfNA, f = is.na, margin = 2, negate = TRUE)
expect_equal(no_na, c(V1 = 3, V2 = 5, V3 = 4))

# Detect =======================================================================
## Find row with NA
expect_equal(detect(mtxNA, f = is.na, margin = 1), c(FALSE, TRUE, TRUE, TRUE, FALSE))
## Find column without any NA
no_na <- detect(mtxNA, f = is.na, margin = 2, negate = TRUE, all = TRUE)
expect_equal(no_na, c(V1 = FALSE, V2 = TRUE, V3 = FALSE))

## Find row with NA
expect_equal(detect(dfNA, f = is.na, margin = 1), c(FALSE, TRUE, TRUE, TRUE, FALSE))
## Find column without any NA
no_na <- detect(dfNA, f = is.na, margin = 2, negate = TRUE, all = TRUE)
expect_equal(no_na, c(V1 = FALSE, V2 = TRUE, V3 = FALSE))

# Discard ======================================================================
# Nothing to remove
clean <- discard_rows(mtx, f = is_zero, all = FALSE)
expect_equal(dim(clean), dim(mtx))

mtx0 <- mtx
mtx0[1, ] <- 0 # Add zeros

# Nothing to remove
clean <- discard_cols(mtx0, f = is_zero, all = TRUE)
expect_equal(dim(clean), dim(mtx0))

mtx0[, 1] <- 0 # Add zeros

# Remove rows
clean_row <- discard_rows(mtx0, f = is_zero, all = TRUE)
expect_true(nrow(clean_row) < nrow(mtx0))
expect_equal(ncol(clean_row), ncol(mtx0))

# Remove columns
clean_col <- discard_cols(mtx0, f = is_zero, all = TRUE)
expect_true(ncol(clean_col) < ncol(mtx0))
expect_equal(nrow(clean_col), nrow(mtx0))

# Keep =========================================================================
# Nothing to keep
clean <- keep_rows(mtx, f = is_zero, all = FALSE)
expect_equal(dim(clean), c(0, 3))

mtx0 <- mtx
mtx0[1, ] <- 0 # Add zeros

# Nothing to keep
clean <- keep_cols(mtx0, f = is_zero, all = TRUE)
expect_equal(dim(clean), c(5, 0))

mtx0[, 1] <- 0 # Add zeros

# Keep all
clean_row <- keep_rows(mtx0, f = is_zero, all = FALSE)
expect_equal(dim(clean_row), dim(mtx0))

clean_col <- keep_cols(mtx0, f = is_zero, all = FALSE)
expect_equal(dim(clean_col), dim(mtx0))

# Remove rows
clean_row <- keep_rows(mtx0, f = is_zero, all = TRUE)
expect_equal(dim(clean_row), c(1, 3))

# Remove columns
clean_col <- keep_cols(mtx0, f = is_zero, all = TRUE)
expect_equal(dim(clean_col), c(5, 1))

# Missing values ===============================================================
## Replace missing values ------------------------------------------------------
clean <- replace_NA(mtxNA, value = 999)
expect_equal(sum(clean == 999), 3)

## Remove missing values -------------------------------------------------------
# Nothing to remove
clean <- remove_NA(mtx, margin = 1)
expect_equal(dim(clean), dim(mtx))

# Remove rows
clean_row <- remove_NA(mtxNA, margin = 1)
expect_true(nrow(clean_row) < nrow(mtxNA))
expect_true(nrow(clean_row) > 0)
expect_equal(ncol(clean_row), ncol(mtxNA))

# Remove columns
clean_col <- remove_NA(mtxNA, margin = 2)
expect_true(ncol(clean_col) < ncol(mtxNA))
expect_true(ncol(clean_col) > 0)
expect_equal(nrow(clean_col), nrow(mtxNA))

# Infinite values ==============================================================
mtxInf <- mtx
mtxInf[sample(1:15, 3, FALSE)] <- Inf # Add infinite values

## Replace infinite values -----------------------------------------------------
clean <- replace_Inf(mtxInf, value = 999)
expect_equal(sum(clean == 999), 3)

## Remove infinite values ------------------------------------------------------
# Nothing to remove
clean <- remove_Inf(mtx, margin = 1)
expect_equal(dim(clean), dim(mtx))

# Remove rows
clean_row <- remove_Inf(mtxInf, margin = 1)
expect_true(nrow(clean_row) < nrow(mtxInf))
expect_equal(ncol(clean_row), ncol(mtxInf))

# Remove columns
clean_col <- remove_Inf(mtxInf, margin = 2)
expect_true(ncol(clean_col) < ncol(mtxInf))
expect_equal(nrow(clean_col), nrow(mtxInf))

# Zeros ========================================================================
mtx0 <- mtx
mtx0[sample(1:15, 3, FALSE)] <- 0 # Add zeros

## Replace zeros ---------------------------------------------------------------
clean <- replace_zero(mtx0, value = 999)
expect_equal(sum(clean == 999), 3)

## Remove zeros ----------------------------------------------------------------
# Nothing to remove
clean <- remove_zero(mtx, margin = 1)
expect_equal(dim(clean), dim(mtx))

# Remove rows
clean_row <- remove_zero(mtx0, margin = 1)
expect_true(nrow(clean_row) < nrow(mtx0))
expect_equal(ncol(clean_row), ncol(mtx0))

# Remove columns
clean_col <- remove_zero(mtx0, margin = 2)
expect_true(ncol(clean_col) < ncol(mtx0))
expect_equal(nrow(clean_col), nrow(mtx0))

# Constant columns =============================================================
df1 <- data.frame(A = 1, B = 1:3)
df2 <- data.frame(B = 1:3)

expect_equal(remove_constant(df1), df2)

df1[1, 1] <- NA # Add NA
expect_equal(remove_constant(df1), df1)

expect_equal(remove_constant(df1, na.rm = TRUE), df2)

# Empty rows/columns ===========================================================
## Compact (numeric) -----------------------------------------------------------
# Nothing to remove
clean <- compact_rows(mtx)
expect_equal(dim(clean), dim(mtx))

mtx0 <- mtx
mtx0[1, ] <- 0 # Add zeros
mtx0[, 1] <- 0 # Add zeros

# Remove rows
clean_row <- compact_rows(mtx0)
expect_equal(nrow(clean_row), 4)
expect_equal(ncol(clean_row), ncol(mtx0))

# Remove columns
clean_col <- compact_cols(mtx0)
expect_equal(ncol(clean_col), 2)
expect_equal(nrow(clean_col), nrow(mtx0))

## Compact (character) ---------------------------------------------------------
char <- as.character(mtx)
dim(char) <- dim(mtx)

# Nothing to remove
clean <- compact_rows(char)
expect_equal(dim(clean), dim(char))

charBlank <- char
charBlank[1, ] <- "" # Add blank
charBlank[, 1] <- "" # Add blank

# Remove rows
clean_row <- compact_rows(charBlank)
expect_equal(nrow(clean_row), 4)
expect_equal(ncol(clean_row), ncol(charBlank))

# Remove columns
clean_col <- compact_cols(charBlank)
expect_equal(ncol(clean_col), 2)
expect_equal(nrow(clean_col), nrow(charBlank))

## Compact (logical) -----------------------------------------------------------
bin <- matrix(c(FALSE, TRUE, TRUE, TRUE, FALSE,
                FALSE, TRUE, FALSE, FALSE, TRUE,
                FALSE, FALSE, FALSE, TRUE, FALSE),
              nrow = 5, ncol = 3)

# Nothing to remove
clean <- compact_rows(bin)
expect_equal(dim(clean), dim(bin))

binNA <- bin
binNA[1, ] <- NA # Add NA
binNA[, 1] <- NA # Add NA

# Remove rows
clean_row <- compact_rows(binNA)
expect_equal(nrow(clean_row), 4)
expect_equal(ncol(clean_row), ncol(binNA))

# Remove columns
clean_col <- compact_cols(binNA)
expect_equal(ncol(clean_col), 2)
expect_equal(nrow(clean_col), nrow(binNA))
