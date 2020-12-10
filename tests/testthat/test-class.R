context("Matrix Classes")

test_that("Empty instance", {
  expect_s4_class(.CountMatrix(), "CountMatrix")
  expect_s4_class(.AbundanceMatrix(), "AbundanceMatrix")
  expect_s4_class(.OccurrenceMatrix(), "OccurrenceMatrix")
  expect_s4_class(.SimilarityMatrix(), "SimilarityMatrix")
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")
  expect_s4_class(.StratigraphicMatrix(), "StratigraphicMatrix")
  expect_s4_class(.MatrixSummary(), "MatrixSummary")
})
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
  # Try NA
  cnd <- catch_conditions(CountMatrix(NA))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
})
test_that("Initialize a AbundanceMatrix instance", {
  # Invalid values
  # Try negative values
  cnd <- catch_conditions(AbundanceMatrix(data = c(-2, 3), nrow = 1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
})
