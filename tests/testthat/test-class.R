test_that("Empty instance", {
  expect_s4_class(.CountMatrix(), "CountMatrix")
  expect_s4_class(.CompositionMatrix(), "CompositionMatrix")
  expect_s4_class(.OccurrenceMatrix(), "OccurrenceMatrix")
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")
  expect_s4_class(.StratigraphicMatrix(), "StratigraphicMatrix")
})
test_that("Initialize a CountMatrix instance", {
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
})
test_that("Initialize a CompositionMatrix instance", {
  # Invalid values
  # Try negative values
  cnd <- catch_conditions(CompositionMatrix(data = c(-2, 3), nrow = 1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
})
test_that("Initialize an IncidenceMatrix instance", {
  # Coerce values to integer
  expect_s4_class(IncidenceMatrix(1.1), "IncidenceMatrix")
})
test_that("Matrix dimension names", {
  cts <- CountMatrix(sample(1:100, 50, TRUE), ncol = 10)
  row_names <- paste0("row", 1:5)
  col_names <- paste0("col", 1:10)

  expect_equal(rownames(cts), row_names)
  expect_equal(colnames(cts), col_names)
  expect_equal(dimnames(cts), list(row_names, col_names))

  # Prior to R 3.4 matrix(nrow = 0, ncol = 0) is logical (?)
  skip_if(getRversion() < "3.6")
  cts <- CountMatrix(data = 0, nrow = 0, ncol = 0)
  expect_null(rownames(cts))
  expect_null(colnames(cts))
})
