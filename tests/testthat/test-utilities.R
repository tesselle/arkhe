context("Utilities")

# Test helpers
test_that("Compact", {
  expect_length(compact(is.null, list("A", NULL, "B")), 2)
})
test_that("Detect", {
  k <- detect(is.na, c(1, 2, NA, 4, 5, NA))
  expect_type(k, "logical")
  expect_equal(sum(k), 2)
})
test_that("Count", {
  k <- count(is.na, c(1, 2, NA, 4, 5, NA))
  expect_type(k, "integer")
  expect_equal(k, 2)
})
test_that("Extract", {
  k <- extract(c("abc123", "def456", "ghi"), "[1-9]{3}")
  expect_type(k, "character")
  expect_length(k, 2)
})
test_that("Function composition", {
  expect_type((sum %o% range)(1:5), "integer")
  expect_equal((sum %o% range)(1:5), 6)
})
test_that("NULL OR operator", {
  expect_type(NULL %||% 1, "double")
  expect_equal(NULL %||% 1, 1)
  expect_equal(0 %||% 1, 0)
})
test_that("Row and column names", {
  mtx <- matrix(sample(1:10, 100, TRUE), ncol = 10)

  X <- make_rownames(mtx)
  expect_equal(rownames(X), as.character(seq_len(10)))

  Y <- make_colnames(mtx)
  expect_equal(colnames(Y), paste0("V", seq_len(10)))

  Z <- make_dimnames(mtx)
  expect_equal(dimnames(Z), list(as.character(seq_len(10)),
                                 paste0("V", seq_len(10))))

  expect_error(make_rownames(LETTERS),
               "A matrix or data.frame is expected.")
  expect_error(make_colnames(LETTERS),
               "A matrix or data.frame is expected.")

  rownames(mtx) <- LETTERS[seq_len(10)]
  A <- rownames_to_column(mtx, factor = TRUE)
  expect_equal(ncol(A), 11)
  expect_true(is.factor(A[[1]]))
  A <- rownames_to_column(mtx, factor = FALSE)
  expect_true(!is.factor(A[[1]]))
  expect_error(rownames_to_column(LETTERS))
})
