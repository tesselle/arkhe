context("Getters and Setters")

test_that("Matrix groups", {
  cts <- CountMatrix(sample(1:100, 75, TRUE), ncol = 5)

  expect_equal(get_groups(cts), character(0))
  expect_false(has_groups(cts))

  set_groups(cts) <- rep(c("A", "B", "C"), each = 5)
  expect_true(has_groups(cts))
  expect_equal(get_groups(cts), rep(c("A", "B", "C"), each = 5))

  set_groups(cts) <- NULL
  expect_false(has_groups(cts))

  # Invalid values
  # Try wrong length
  cnd <- catch_conditions(set_groups(cts) <- LETTERS)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 15; not 26", cnd[[1]]$message))
})
test_that("AbundanceMatrix totals", {
  cts <- matrix(sample(1:100, 100, TRUE), ncol = 10)
  freq <- as_abundance(cts)

  expect_equivalent(get_totals(freq), rowSums(cts))

  set_totals(freq) <- seq_len(10)
  expect_equivalent(get_totals(freq), seq_len(10))

  # Invalid values
  # Try negative values
  cnd <- catch_conditions(set_totals(freq) <- -seq_len(10))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(set_totals(freq) <- Inf)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(set_totals(freq) <- NA_integer_)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
})
test_that("SimilarityMatrix method", {
  sim <- SimilarityMatrix()

  expect_equal(get_method(sim), "unknown")

  set_method(sim) <- "xxx"
  expect_equal(get_method(sim), "xxx")

  # Invalid values
  # Try more than one value
  cnd <- catch_conditions(set_method(sim) <- c("A", "B"))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be a scalar", cnd[[1]]$message))
})
test_that("OccurrenceMatrix number of observations", {
  cts <- matrix(sample(0:1, 100, TRUE), ncol = 10)
  occ <- as_occurrence(cts)

  expect_equal(get_n(occ), nrow(cts))
})
