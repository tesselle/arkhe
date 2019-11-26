context("Matrix classes")

# Matrix =======================================================================
test_that("Initialize a Matrix instance", {
  options("verbose" = FALSE)
  # Empty instence
  expect_s4_class(.Matrix(), "Matrix")
  expect_s4_class(Matrix(1), "Matrix")

  cnd <- catch_conditions(Matrix(matrix(NA, 1, 1)))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(Matrix(matrix(NaN, 1, 1)))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(Matrix(matrix(Inf, 1, 1)))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))

  cnd <- catch_conditions(Matrix(id = NA_character_))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))

  cnd <- catch_conditions(Matrix(1, id = "a"))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))
})

# Numeric matrix ===============================================================
test_that("Initialize a NumericMatrix instance", {
  options("verbose" = FALSE)
  # Empty instence
  expect_s4_class(.NumericMatrix(), "NumericMatrix")
  expect_s4_class(NumericMatrix(1), "NumericMatrix")

  # Try logical
  cnd <- catch_conditions(NumericMatrix(TRUE))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be numeric; not logical", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(NumericMatrix("X"))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(NumericMatrix(NA))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(NumericMatrix(NaN))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(NumericMatrix(Inf))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("NumericMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(NumericMatrix(1))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }
})

## Count matrix ----------------------------------------------------------------
test_that("Initialize a CountMatrix instance", {
  options("verbose" = FALSE)
  expect_s4_class(.AbsoluteFrequencyMatrix(), "AbsoluteFrequencyMatrix")
  expect_s4_class(CountMatrix(1), "AbsoluteFrequencyMatrix")

  # Try relative frequencies
  cnd <- catch_conditions(CountMatrix(1.1))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must contain whole numbers", cnd[[1]]$message))
  # Try logical
  cnd <- catch_conditions(CountMatrix(TRUE))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be numeric; not logical", cnd[[1]]$message))
  # Try negative values
  cnd <- catch_conditions(CountMatrix(-1))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(CountMatrix("X"))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(CountMatrix(NA))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(CountMatrix(NaN))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(CountMatrix(Inf))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("CountMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(CountMatrix(1))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }

  count_matrix1 <- CountMatrix(
    data = sample(0:10, 100, TRUE),
    ncol = 20
  )
  expect_equal(dim(count_matrix1), c(5, 20))
  expect_equal(dimnames(count_matrix1),
               list(as.character(1:5), paste0("V", 1:20)))

  count_matrix2 <- CountMatrix(
    data = sample(0:10, 100, TRUE),
    nrow = 20,
    dimnames = list(NULL, LETTERS[1:5])
  )
  expect_equal(dim(count_matrix2), c(20, 5))
  expect_equal(dimnames(count_matrix2),
               list(as.character(1:20), LETTERS[1:5]))
})

## Frequency matrix ------------------------------------------------------------
test_that("Initialize a RelativeFrequencyMatrix instance", {
  options("verbose" = FALSE)
  expect_s4_class(.RelativeFrequencyMatrix(), "RelativeFrequencyMatrix")
  expect_s4_class(RelativeFrequencyMatrix(1), "RelativeFrequencyMatrix")

  id <- generate_uuid()
  # Try missing totals
  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix(1), id = id, totals = c(1,2)))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be of length 1; not 2", cnd[[1]]$message))

  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix(c(1, 2), nrow = 2), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must have constant row sums", cnd[[1]]$message))

  # Try negative values
  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix(-2), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix("X"), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix(NA), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix(NaN), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.RelativeFrequencyMatrix(matrix(Inf), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("RelativeFrequencyMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(RelativeFrequencyMatrix(1))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }

  freq_matrix1 <- RelativeFrequencyMatrix(
    data = sample(0:10, 100, TRUE),
    ncol = 20
  )
  expect_length(freq_matrix1@totals, 5)
  expect_equal(dim(freq_matrix1), c(5, 20))
  expect_equal(dimnames(freq_matrix1),
               list(as.character(1:5), paste0("V", 1:20)))

  freq_matrix2 <- RelativeFrequencyMatrix(
    data = sample(0:10, 100, TRUE),
    nrow = 20,
    dimnames = list(NULL, LETTERS[1:5])
  )
  expect_equal(dim(freq_matrix2), c(20, 5))
  expect_equal(dimnames(freq_matrix2),
               list(as.character(1:20), LETTERS[1:5]))
})

## Co-occurrenceMatrix matrix --------------------------------------------------
test_that("OccurrenceMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(OccurrenceMatrix(1))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }
})

## Similarity matrix -----------------------------------------------------------
test_that("SimilarityMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(SimilarityMatrix(1))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }
})

# Logical matrix ===============================================================
test_that("Initialize a LogicalMatrix instance", {
  options("verbose" = FALSE)
  expect_s4_class(.LogicalMatrix(), "LogicalMatrix")
  expect_s4_class(LogicalMatrix(TRUE), "LogicalMatrix")

  id <- generate_uuid()
  # Try count data
  cnd <- catch_conditions(.LogicalMatrix(1L, id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be logical; not integer", cnd[[1]]$message))
  # Try frequency data
  cnd <- catch_conditions(.LogicalMatrix(1.1, id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be logical; not double", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.LogicalMatrix("X", id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be logical; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.LogicalMatrix(NA, id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.LogicalMatrix(NaN, id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.LogicalMatrix(Inf, id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("LogicalMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(LogicalMatrix(TRUE))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }
})
test_that("Initialize a IncidenceMatrix instance", {
  options("verbose" = FALSE)
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")
  expect_s4_class(IncidenceMatrix(TRUE), "IncidenceMatrix")

  id <- generate_uuid()
  # Try count data
  cnd <- catch_conditions(.IncidenceMatrix(matrix(1L), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be logical; not integer", cnd[[1]]$message))
  # Try frequency data
  cnd <- catch_conditions(.IncidenceMatrix(matrix(1.1), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be logical; not double", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.IncidenceMatrix(matrix("X"), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must be logical; not character.", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.IncidenceMatrix(matrix(NA), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.IncidenceMatrix(matrix(NaN), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.IncidenceMatrix(matrix(Inf), id = id))
  expect_s3_class(cnd[[1]], "codex_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("IncidenceMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(IncidenceMatrix(TRUE))
  for (i in length(cnd)) {
    expect_s3_class(cnd[[i]], "codex_message_class")
  }

  incid_matrix1 <- IncidenceMatrix(
    data = sample(0:1, 100, TRUE),
    ncol = 20
  )
  expect_equal(dim(incid_matrix1), c(5, 20))
  expect_equal(dimnames(incid_matrix1),
               list(as.character(1:5), paste0("V", 1:20)))

  incid_matrix2 <- IncidenceMatrix(
    data = as.logical(sample(0:1, 100, TRUE)),
    nrow = 20,
    dimnames = list(LETTERS[1:20], NULL)
  )
  expect_equal(dim(incid_matrix2), c(20, 5))
  expect_equal(dimnames(incid_matrix2),
               list(LETTERS[1:20], paste0("V", 1:5)))
})
