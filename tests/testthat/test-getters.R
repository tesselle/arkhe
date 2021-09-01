test_that("AbundanceMatrix groups", {
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
test_that("AbundanceMatrix samples", {
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
test_that("AbundanceMatrix dates", {
  cts <- CountMatrix(sample(1:100, 75, TRUE), ncol = 5)
  tpq <- sample(1301:1400, 15, replace = TRUE)
  taq <- sample(1451:1500, 15, replace = TRUE)
  dates <- list(tpq = tpq, taq = taq)

  expect_equal(get_dates(cts), data.frame(tpq = integer(0), taq = integer(0)))

  set_dates(cts) <- dates
  expect_true(has_dates(cts))
  expect_equal(get_dates(cts), as.data.frame(dates), ignore_attr = TRUE)
  expect_equal(get_tpq(cts), dates$tpq)
  expect_equal(get_taq(cts), dates$taq)

  set_dates(cts) <- NULL
  expect_false(has_dates(cts))
  expect_equal(get_dates(cts), data.frame(tpq = integer(0), taq = integer(0)))

  set_tpq(cts) <- tpq
  set_taq(cts) <- taq
  expect_true(has_dates(cts))

  set_tpq(cts) <- NULL
  set_taq(cts) <- NULL
  expect_false(has_dates(cts))

  # Invalid values
  # Try unnamed list
  names(dates) <- NULL
  cnd <- catch_conditions(set_dates(cts) <- dates)
  expect_s3_class(cnd[[1]], "simpleError")
  expect_true(grepl("but does not have components", cnd[[1]]$message))
  # Try wrong order
  names(dates) <- c("taq", "tpq")
  cnd <- catch_conditions(set_dates(cts) <- dates)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be lower than", cnd[[1]]$message))
  # Try wrong length
  cnd <- catch_conditions(set_dates(cts) <- 1:5)
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 15; not 5", cnd[[1]]$message))
})
test_that("AbundanceMatrix totals", {
  mtx <- matrix(sample(1:100, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)
  freq <- as_composition(cts)

  expect_equal(get_totals(cts), rowSums(mtx), ignore_attr = TRUE)
  expect_equal(get_totals(freq), rowSums(mtx), ignore_attr = TRUE)

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
