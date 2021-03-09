# matrix =======================================================================
test_that("matrix > *Matrix", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  freq <- prop.table(cts, 1)
  incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)

  # CountMatrix
  A <- as_count(cts)
  expect_s4_class(A, "CountMatrix")
  expect_equal(A@.Data, cts, ignore_attr = TRUE)

  # CompositionMatrix
  B <- as_composition(cts)
  expect_s4_class(B, "CompositionMatrix")
  expect_equal(B@.Data, freq, ignore_attr = TRUE)

  # IncidenceMatrix
  D <- as_incidence(incid)
  expect_s4_class(D, "IncidenceMatrix")
  expect_equal(D@.Data, incid, ignore_attr = TRUE)

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

  # CompositionMatrix
  B <- as_composition(df_count)
  expect_s4_class(B, "CompositionMatrix")
  expect_equal(as.data.frame(B), df_freq, ignore_attr = TRUE)

  # IncidenceMatrix
  C <- as_incidence(df_incid)
  expect_equal(as.data.frame(C), df_incid, ignore_attr = TRUE)
  expect_s4_class(as_incidence(df_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(df_freq), "IncidenceMatrix")

  # OccurrenceMatrix
  expect_s4_class(as_occurrence(df_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_freq), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_incid), "OccurrenceMatrix")
})
# *Matrix ======================================================================
test_that("CountMatrix <> CompositionMatrix", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)

  counts1 <- as_count(cts)
  freq1 <- as_composition(counts1)
  counts2 <- as_count(freq1)
  expect_equal(counts1, counts2)
})
test_that("DataMatrix > long", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  counts <- as(cts, "CountMatrix")

  A <- as_long(counts, factor = TRUE, reverse = FALSE)
  expect_equal(dim(A), c(50, 5))
  expect_s3_class(A$row, "factor")
  expect_s3_class(A$column, "factor")

  B <- as_long(counts, factor = TRUE, reverse = TRUE)
  expect_equal(rev(levels(A$row)), levels(B$row))
})
test_that("DataMatrix > features", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  freq <- as_composition(cts)
  feat <- as_features(freq)

  expect_equal(dim(feat), c(5, 12))
})
# Autodetect ===================================================================
test_that("Autodetect", {
  spl <- LETTERS[1:5]
  grp <- c("a", "a", "b", "b", "c")

  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  cts <- as.data.frame(cts)
  cts$sample <- spl
  cts$group <- grp

  options(arkhe.autodetect = TRUE)
  counts <- as_count(cts)
  expect_equal(get_samples(counts), spl)
  expect_equal(get_groups(counts), grp)

  freq <- as_composition(cts)
  expect_equal(get_samples(freq), spl)
  expect_equal(get_groups(freq), grp)

  incid <- as_incidence(cts)
  expect_equal(get_samples(incid), spl)
  expect_equal(get_groups(incid), grp)

  options(arkhe.autodetect = FALSE)
  counts <- as_count(cts)
  expect_equal(get_samples(counts), rownames(counts))
  expect_equal(get_groups(counts), character(0))
})
