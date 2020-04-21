context("Matrix classes")

test_that("Initialize a CountMatrix instance", {
  expect_s4_class(.CountMatrix(), "CountMatrix")

  # Try missing row names
  cnd <- catch_conditions(.CountMatrix(data = 1L, size = c(1L, 1L),
                                       column_names = "X"))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 1; not 0", cnd[[1]]$message))
  # Try missing column names
  cnd <- catch_conditions(.CountMatrix(data = 1L, size = c(1L, 1L),
                                       row_names = "X"))

  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 1; not 0", cnd[[1]]$message))

  # Try relative frequencies
  expect_s4_class(CountMatrix(1.1), "CountMatrix")
  # Try logical
  expect_s4_class(CountMatrix(TRUE), "CountMatrix")
  # Try negative values
  cnd <- catch_conditions(CountMatrix(-1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(CountMatrix(NA))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(CountMatrix(NaN))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
})
test_that("CountMatrix constructor", {
  count_matrix1 <- CountMatrix(data = sample(0:10, 100, TRUE), ncol = 20)
  expect_equal(dim(count_matrix1), c(5, 20))
  expect_equal(dimnames(count_matrix1),
               list(paste0("row", 1:5), paste0("col", 1:20)))
})

## Frequency matrix ------------------------------------------------------------
test_that("Initialize a AbundanceMatrix instance", {
  expect_s4_class(.AbundanceMatrix(), "AbundanceMatrix")

  # Try wrong total
  cnd <- catch_conditions(AbundanceMatrix(1, totals = c(1, 2)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 1; not 2", cnd[[1]]$message))

  # Try negative values
  cnd <- catch_conditions(AbundanceMatrix(-2))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(AbundanceMatrix(NA))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(AbundanceMatrix(NaN))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
})
test_that("AbundanceMatrix constructor", {
  expect_s4_class(AbundanceMatrix(), "AbundanceMatrix")

  freq_matrix1 <- AbundanceMatrix(
    data = sample(0:10, 100, TRUE),
    ncol = 20
  )
  expect_length(freq_matrix1@totals, 5)
  expect_equal(dim(freq_matrix1), c(5, 20))
  expect_equal(dimnames(freq_matrix1),
               list(paste0("row", 1:5), paste0("col", 1:20)))

  freq_matrix2 <- AbundanceMatrix(
    data = sample(0:10, 100, TRUE),
    nrow = 20,
    dimnames = list(NULL, LETTERS[1:5])
  )
  expect_equal(dim(freq_matrix2), c(20, 5))
  expect_equal(dimnames(freq_matrix2),
               list(paste0("row", 1:20), LETTERS[1:5]))
})

## Co-occurrenceMatrix matrix --------------------------------------------------
test_that("Initialize a OccurrenceMatrix instance", {
  expect_s4_class(.OccurrenceMatrix(), "OccurrenceMatrix")
  expect_identical(.OccurrenceMatrix()@n, 0L)
})

## Similarity matrix -----------------------------------------------------------
test_that("Initialize a SimilarityMatrix instance", {
  expect_s4_class(.SimilarityMatrix(), "SimilarityMatrix")
  expect_identical(.SimilarityMatrix()@method, "unknown")
})

# Logical matrix ===============================================================
test_that("Initialize a IncidenceMatrix instance", {
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")

  # Try NA
  cnd <- catch_conditions(IncidenceMatrix(NA))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(IncidenceMatrix(NaN))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
})
test_that("IncidenceMatrix constructor", {
  incid_matrix1 <- IncidenceMatrix(
    data = sample(0:1, 100, TRUE),
    ncol = 20
  )
  expect_equal(dim(incid_matrix1), c(5, 20))
  expect_equal(dimnames(incid_matrix1),
               list(paste0("row", 1:5), paste0("col", 1:20)))

  incid_matrix2 <- IncidenceMatrix(
    data = as.logical(sample(0:1, 100, TRUE)),
    nrow = 20,
    dimnames = list(LETTERS[1:20], NULL)
  )
  expect_equal(dim(incid_matrix2), c(20, 5))
  expect_equal(dimnames(incid_matrix2),
               list(LETTERS[1:20], paste0("col", 1:5)))
})
