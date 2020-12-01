context("Matrix classes")

test_that("Empty instance", {
  expect_s4_class(.CountMatrix(), "CountMatrix")
  expect_s4_class(.AbundanceMatrix(), "AbundanceMatrix")
  expect_s4_class(.OccurrenceMatrix(), "OccurrenceMatrix")
  expect_s4_class(.SimilarityMatrix(), "SimilarityMatrix")
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")
})
# CountMatrix ==================================================================
test_that("CountMatrix", {
  # Coerce values to integer
  # Try double
  expect_s4_class(CountMatrix(1.1), "CountMatrix")
  # Try logical
  expect_s4_class(CountMatrix(TRUE), "CountMatrix")

  # Invalid values
  # Try negative values
  cnd <- catch_conditions(CountMatrix(-1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(CountMatrix(Inf))
  expect_s3_class(cnd[[2]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[2]]$message))
  # Try NA
  cnd <- catch_conditions(CountMatrix(NA))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
})
test_that("CountMatrix constructor", {
  expect_s4_class(CountMatrix(), "CountMatrix")

  mtx <- CountMatrix(data = sample(0:10, 100, TRUE), ncol = 20)
  expect_equal(dim(mtx), c(5, 20))
  expect_equal(dimnames(mtx), list(paste0("row", 1:5), paste0("col", 1:20)))
})

# AbundanceMatrix ==============================================================
test_that("Initialize a AbundanceMatrix instance", {
  # Invalid values
  # Try negative values
  cnd <- catch_conditions(AbundanceMatrix(data = c(-2, 3), nrow = 1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
})
test_that("AbundanceMatrix constructor", {
  expect_s4_class(AbundanceMatrix(), "AbundanceMatrix")

  mtx1 <- AbundanceMatrix(data = sample(0:10, 100, TRUE), ncol = 20)
  expect_length(mtx1@totals, 5)
  expect_equal(dim(mtx1), c(5, 20))
  expect_equal(dimnames(mtx1), list(paste0("row", 1:5), paste0("col", 1:20)))

  mtx2 <- AbundanceMatrix(data = sample(0:10, 100, TRUE), nrow = 20,
                          dimnames = list(NULL, LETTERS[1:5]))
  expect_equal(dim(mtx2), c(20, 5))
  expect_equal(dimnames(mtx2), list(paste0("row", 1:20), LETTERS[1:5]))
})

# SimilarityMatrix =============================================================
test_that("SimilarityMatrix constructor", {
  expect_s4_class(SimilarityMatrix(), "SimilarityMatrix")
  expect_identical(SimilarityMatrix()@method, "unknown")
})

# IncidenceMatrix ==============================================================
test_that("IncidenceMatrix constructor", {
  mtx1 <- IncidenceMatrix(data = sample(0:1, 100, TRUE), ncol = 20)
  expect_equal(dim(mtx1), c(5, 20))
  expect_equal(dimnames(mtx1), list(paste0("row", 1:5), paste0("col", 1:20)))

  mtx2 <- IncidenceMatrix(data = as.logical(sample(0:1, 100, TRUE)), nrow = 20,
                          dimnames = list(LETTERS[1:20], NULL))
  expect_equal(dim(mtx2), c(20, 5))
  expect_equal(dimnames(mtx2),
               list(LETTERS[1:20], paste0("col", 1:5)))
})
