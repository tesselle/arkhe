mtx <- matrix(c(8L, 10L, 1L, 10L, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

# Compact (numeric) ============================================================
## Nothing to remove
clean <- compact_rows(mtx)
expect_equal(dim(clean), dim(mtx))

mtx0 <- mtx
mtx0[1, ] <- 0 # Add zeros
mtx0[, 1] <- 0 # Add zeros

## Remove rows
clean_row <- compact_rows(mtx0)
expect_equal(nrow(clean_row), 4)
expect_equal(ncol(clean_row), ncol(mtx0))

## Remove columns
clean_col <- compact_columns(mtx0)
expect_equal(ncol(clean_col), 2)
expect_equal(nrow(clean_col), nrow(mtx0))

# Compact (character) ==========================================================
char <- as.character(mtx)
dim(char) <- dim(mtx)

## Nothing to remove
clean <- compact_rows(char)
expect_equal(dim(clean), dim(char))

charBlank <- char
charBlank[1, ] <- "" # Add blank
charBlank[, 1] <- "" # Add blank

## Remove rows
clean_row <- compact_rows(charBlank)
expect_equal(nrow(clean_row), 4)
expect_equal(ncol(clean_row), ncol(charBlank))

## Remove columns
clean_col <- compact_columns(charBlank)
expect_equal(ncol(clean_col), 2)
expect_equal(nrow(clean_col), nrow(charBlank))

# Compact (logical) ============================================================
bin <- matrix(c(FALSE, TRUE, TRUE, TRUE, FALSE,
                FALSE, TRUE, FALSE, FALSE, TRUE,
                FALSE, FALSE, FALSE, FALSE, FALSE),
              nrow = 5, ncol = 3)

## Nothing to remove
clean <- compact_rows(bin)
expect_equal(nrow(clean), 4)
clean <- compact_columns(bin)
expect_equal(ncol(clean), 2)

binNA <- bin
binNA[1, 1] <- NA # Add NA
binNA[5, 3] <- NA # Add NA

## Remove rows
clean_row <- compact_rows(binNA) # Nothing to remove
expect_equal(dim(clean_row), dim(binNA))
clean_row <- compact_rows(binNA, na.rm = TRUE)
expect_equal(nrow(clean_row), 4)
expect_equal(ncol(clean_row), ncol(binNA))

## Remove columns
clean_col <- compact_columns(binNA) # Nothing to remove
expect_equal(dim(clean_col), dim(binNA))
clean_col <- compact_columns(binNA, na.rm = TRUE)
expect_equal(ncol(clean_col), 2)
expect_equal(nrow(clean_col), nrow(binNA))
