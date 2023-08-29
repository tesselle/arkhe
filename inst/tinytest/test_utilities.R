# Extract ======================================================================
k <- arkhe:::extract(c("abc123", "def456", "ghi"), "[1-9]{3}")
expect_equal(k, c("123", "456"))

# Function composition =========================================================
expect_equal(arkhe:::`%o%`(sum, range)(1:5), 6)

# NULL OR operator =============================================================
expect_equal(arkhe:::`%||%`(NULL, 1), 1)
expect_equal(arkhe:::`%||%`(0, 1), 0)
