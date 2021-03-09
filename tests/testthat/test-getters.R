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
test_that("Matrix samples", {
  cts <- CountMatrix(sample(1:100, 75, TRUE), ncol = 5)

  expect_equal(get_samples(cts), paste0("row", 1:15))

  set_samples(cts) <- rep(c("A", "B", "C"), each = 5)
  expect_equal(get_samples(cts), rep(c("A", "B", "C"), each = 5))

  set_samples(cts) <- NULL
  expect_equal(get_samples(cts), paste0("row", 1:15))

  # Invalid values
  # Try wrong length
  cnd <- catch_conditions(set_samples(cts) <- LETTERS)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 15; not 26", cnd[[1]]$message))
})
test_that("CompositionMatrix totals", {
  cts <- matrix(sample(1:100, 100, TRUE), ncol = 10)
  freq <- as_composition(cts)

  expect_equal(get_totals(freq), rowSums(cts))

  set_totals(freq) <- seq_len(10)
  expect_equal(get_totals(freq), seq_len(10))

  # Invalid values
  # Try negative values
  cnd <- catch_conditions(set_totals(freq) <- -seq_len(10))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try wrong length
  cnd <- catch_conditions(set_totals(freq) <- 1)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 10; not 1", cnd[[1]]$message))
})
test_that("OccurrenceMatrix number of observations", {
  cts <- matrix(sample(0:1, 100, TRUE), ncol = 10)
  occ <- as_occurrence(cts)

  expect_equal(get_totals(occ), nrow(cts))
})
