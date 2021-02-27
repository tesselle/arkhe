# matrix =======================================================================
test_that("matrix > *Matrix", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  freq <- prop.table(cts, 1)
  incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)

  # CountMatrix
  A <- as_count(cts)
  expect_s4_class(A, "CountMatrix")
  expect_equal(A@.Data, cts, ignore_attr = TRUE)

  # AbundanceMatrix
  B <- as_abundance(cts)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equal(B@.Data, freq, ignore_attr = TRUE)

  # IncidenceMatrix
  C <- as_incidence(incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equal(C@.Data, incid, ignore_attr = TRUE)

  expect_s4_class(as_incidence(cts), "IncidenceMatrix")
  expect_s4_class(as_incidence(freq), "IncidenceMatrix")

  # OccurrenceMatrix
  expect_s4_class(as_occurrence(cts), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(freq), "OccurrenceMatrix")
})

# data.frame ===================================================================
test_that("data.frame <> *Matrix", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  freq <- prop.table(cts, 1)
  incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)

  df_count <- as.data.frame(cts)
  df_freq <- as.data.frame(freq)
  df_incid <- as.data.frame(incid)

  # CountMatrix
  A <- as_count(df_count)
  expect_s4_class(A, "CountMatrix")
  expect_equal(as.data.frame(A), df_count, ignore_attr = TRUE)

  # AbundanceMatrix
  B <- as_abundance(df_count)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equal(as.data.frame(B), df_freq, ignore_attr = TRUE)

  # IncidenceMatrix
  C <- as_incidence(df_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equal(as.data.frame(C), df_incid, ignore_attr = TRUE)
  expect_s4_class(as_incidence(df_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(df_freq), "IncidenceMatrix")

  # OccurrenceMatrix
  expect_s4_class(as_occurrence(df_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_freq), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_incid), "OccurrenceMatrix")
})

# *Matrix ======================================================================
test_that("CountMatrix <> AbundanceMatrix", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)

  counts1 <- as_count(cts)
  freq1 <- as_abundance(counts1)
  counts2 <- as_count(freq1)
  expect_equal(counts1, counts2)
})
test_that("DataMatrix > long", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  counts <- as(cts, "CountMatrix")

  A <- as_long(counts, factor = TRUE)
  expect_equal(dim(A), c(50, 5))
  expect_s3_class(A$row, "factor")
  expect_s3_class(A$column, "factor")
})
test_that("DataMatrix > features", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  freq <- as_abundance(cts)
  feat <- as_features(freq)

  expect_equal(dim(feat), c(5, 12))
})
