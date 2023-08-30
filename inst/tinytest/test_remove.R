mtx <- matrix(c(8L, 10L, 1L, 10L, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

mtxZero <- mtxInf <- mtxNA <- mtx
mtxNA[c(1, 4, 13)] <- NA
mtxInf[c(1, 4, 13)] <- Inf
mtxZero[c(1, 4, 13)] <- 0

mtxEmpty <- apply(X = mtx, MARGIN = 2, FUN = as.character)
mtxEmpty[c(1, 4, 13)] <- ""

# Missing values ===============================================================
## Nothing to remove
clean <- remove_NA(mtx, margin = 1)
expect_equal(dim(clean), dim(mtx))

## Remove rows
clean_row <- remove_NA(mtxNA, margin = 1)
expect_equal(dim(clean_row), c(2, 3))

## Remove columns
clean_col <- remove_NA(mtxNA, margin = 2)
expect_equal(dim(clean_col), c(5, 1))

# Infinite values ==============================================================
## Nothing to remove
clean <- remove_Inf(mtx, margin = 1)
expect_equal(dim(clean), dim(mtx))

## Remove rows
clean_row <- remove_Inf(mtxInf, margin = 1)
expect_equal(dim(clean_row), c(2, 3))

## Remove columns
clean_col <- remove_Inf(mtxInf, margin = 2)
expect_equal(dim(clean_col), c(5, 1))

# Zeros ========================================================================
## Nothing to remove
clean <- remove_zero(mtxZero, margin = 2, all = TRUE)
expect_equal(clean, mtxZero)

## Remove rows
clean_row <- remove_zero(mtxZero, margin = 1)
expect_equal(dim(clean_row), c(2, 3))

## Remove columns
clean_col <- remove_zero(mtxZero, margin = 2)
expect_equal(dim(clean_col), c(5, 1))

# Empty strings ================================================================
## Nothing to remove
clean <- remove_empty(mtxEmpty, margin = 2, all = TRUE)
expect_equal(clean, mtxEmpty)

## Remove rows
clean_row <- remove_empty(mtxEmpty, margin = 1)
expect_equal(dim(clean_row), c(2, 3))

## Remove columns
clean_col <- remove_empty(mtxEmpty, margin = 2)
expect_equal(dim(clean_col), c(5, 1))

# Constant columns =============================================================
df1 <- data.frame(A = 1, B = 1:3)
df2 <- data.frame(B = 1:3)

expect_equal(remove_constant(df1), df2)

df1[1, 1] <- NA # Add NA
expect_equal(remove_constant(df1), df1)

expect_equal(remove_constant(df1, na.rm = TRUE), df2)
