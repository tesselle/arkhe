## Read fake data
artefacts <- read.csv(system.file("tinytest/fake.csv", package = "arkhe"))

# Missing values ===============================================================
## Nothing to remove
no_rows <- remove_NA(artefacts, margin = 1, all = TRUE)
expect_equal(dim(no_rows), dim(artefacts))
no_cols <- remove_NA(artefacts, margin = 2, all = TRUE)
expect_equal(dim(no_cols), dim(artefacts))

## Remove rows
clean_row <- remove_NA(artefacts, margin = 1, all = FALSE)
expect_equal(dim(clean_row), dim(artefacts) - c(35, 0))

## Remove columns
clean_col <- remove_NA(artefacts, margin = 2, all = FALSE)
expect_equal(dim(clean_col), dim(artefacts) - c(0, 4))

# Infinite values ==============================================================
## Nothing to remove
no_rows <- remove_Inf(artefacts, margin = 1, all = TRUE)
expect_equal(dim(no_rows), dim(artefacts))
no_cols <- remove_Inf(artefacts, margin = 2, all = TRUE)
expect_equal(dim(no_cols), dim(artefacts))

## Remove rows
clean_row <- remove_Inf(artefacts, margin = 1, all = FALSE)
expect_equal(dim(clean_row), dim(artefacts) - c(1, 0))

## Remove columns
clean_col <- remove_Inf(artefacts, margin = 2, all = FALSE)
expect_equal(dim(clean_col), dim(artefacts) - c(0, 2))

# Zeros ========================================================================
## Nothing to remove
no_rows <- remove_zero(artefacts, margin = 1, all = TRUE)
expect_equal(dim(no_rows), dim(artefacts))
no_cols <- remove_zero(artefacts, margin = 2, all = TRUE)
expect_equal(dim(no_cols), dim(artefacts))

## Remove rows
clean_row <- remove_zero(artefacts, margin = 1, all = FALSE)
expect_equal(dim(clean_row), dim(artefacts) - c(19, 0))

## Remove columns
clean_col <- remove_zero(artefacts, margin = 2, all = FALSE)
expect_equal(dim(clean_col), dim(artefacts) - c(0, 2))

# Empty strings ================================================================
## Nothing to remove
no_rows <- remove_empty(artefacts, margin = 1, all = TRUE)
expect_equal(dim(no_rows), dim(artefacts))
no_cols <- remove_empty(artefacts, margin = 2, all = TRUE)
expect_equal(dim(no_cols), dim(artefacts))

## Remove rows
clean_row <- remove_empty(artefacts, margin = 1, all = FALSE)
expect_equal(dim(clean_row), dim(artefacts) - c(10, 0))

## Remove columns
clean_col <- remove_empty(artefacts, margin = 2, all = FALSE)
expect_equal(dim(clean_col), dim(artefacts) - c(0, 1))

# Constant columns =============================================================
df1 <- data.frame(A = 1, B = 1:3)
df2 <- data.frame(B = 1:3)

expect_equal(remove_constant(df1), df2)

df1[1, 1] <- NA # Add NA
expect_equal(remove_constant(df1), df1)

expect_equal(remove_constant(df1, na.rm = TRUE), df2)
