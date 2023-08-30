mtx <- matrix(c(8L, 10L, 1L, 10L, 2L, 5L, 10L, 10L, 3L, 3L, 6L, 8L, 4L, 4L, 8L),
              nrow = 5, ncol = 3)

mtxZero <- mtxInf <- mtxNA <- mtx
mtxNA[c(1, 4, 13)] <- NA
mtxInf[c(1, 4, 13)] <- Inf
mtxZero[c(1, 4, 13)] <- 0

mtxEmpty <- apply(X = mtx, MARGIN = 2, FUN = as.character)
mtxEmpty[c(1, 4, 13)] <- ""

# Missing values ===============================================================
clean <- replace_NA(mtxNA, value = 999)
expect_equal(sum(clean == 999), 3)

# Infinite values ==============================================================
clean <- replace_Inf(mtxInf, value = 999)
expect_equal(sum(clean == 999), 3)

# Zeros ========================================================================
clean <- replace_zero(mtxZero, value = 999)
expect_equal(sum(clean == 999), 3)

# Empty strings ================================================================
clean <- replace_empty(mtxEmpty, value = "XXX")
expect_equal(sum(clean == "XXX"), 3)

df <- data.frame(
  V1 = c("A", "", "B", "C", "D"),
  V2 = c(1L, Inf, 3L, 4L, 5L),
  V3 = c(0.0, 1.1, NA, 3.3, 4.4)
)
df <- replace_NA(df, value = 999)
df <- replace_Inf(df, value = 888L)
df <- replace_zero(df, value = 777)
df <- replace_empty(df, value = "XXX")

expect_equal(df, data.frame(V1 = c("A", "XXX", "B", "C", "D"),
                            V2 = c(1L, 888L, 3L, 4L, 5L),
                            V3 = c(777.0, 1.1, 999.0, 3.3, 4.4)))
