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
  expect_identical(make_names(x = NULL, n = NULL), character(0))
  expect_identical(make_names(x = NULL, n = 10, prefix = "R"),
                   paste0("R", 1:10))
  expect_identical(make_names(x = c("A", "A", "B")), c("A", "A_1", "B"))

  mtx <- matrix(sample(1:10, 100, TRUE), ncol = 10)
  expect_identical(make_dimnames(mtx), list(paste0("row", 1:10),
                                            paste0("col", 1:10)))

  A <- rownames_to_column(mtx, factor = FALSE)
  expect_type(A[[1]], "character")
  expect_equal(ncol(A), 11)
  expect_equal(A[[1]], paste0("row", 1:10))
  expect_error(rownames_to_column(LETTERS))

  rownames(mtx) <- LETTERS[1:10]

  expect_identical(make_dimnames(mtx), list(LETTERS[1:10], paste0("col", 1:10)))

  B <- rownames_to_column(mtx, factor = TRUE)
  expect_true(is.factor(B[[1]]))
  expect_equal(ncol(B), 11)
  expect_equal(B[[1]], as.factor(LETTERS[1:10]))
})
test_that("Row and column index", {
  mtx <- matrix(data = 1:6, nrow = 2)

  expect_true(all(index_by_row(c(2, 3)) == row(mtx)))
  expect_error(index_by_row(1))
  expect_error(index_by_row("X"))

  expect_true(all(index_by_column(c(2, 3)) == col(mtx)))
  expect_error(index_by_column(1))
  expect_error(index_by_column("X"))
})
