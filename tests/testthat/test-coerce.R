test_that("Autodetect", {
  samples <- rep(c("a", "b", "c", "d", "e"), each = 3)
  groups <- rep(c("A", "B", "C"), each = 5)
  dates <- sample(1400:1451, 15, TRUE)
  tpq <- sample(1301:1400, 15, TRUE)
  taq <- sample(1451:1500, 15, TRUE)

  X <- matrix(data = sample(0:10, 75, TRUE), nrow = 15, ncol = 5)
  Y <- data.frame(samples = samples, groups = groups, dates = dates,
                  tpq = tpq, taq = taq, X)

  options(arkhe.autodetect = FALSE)
  Z <- as_count(Y)
  expect_equal(get_samples(Z), rownames(Y))
  expect_equal(get_groups(Z), character(0))
  expect_equal(get_dates(Z), integer(0))
  expect_equal(get_terminus(Z), data.frame(tpq = integer(0), taq = integer(0)))

  options(arkhe.autodetect = TRUE)
  Z <- as_count(Y)
  expect_equal(get_samples(Z), samples)
  expect_equal(get_groups(Z), groups)
  expect_equal(get_dates(Z), dates)
  expect_equal(get_terminus(Z), data.frame(tpq = tpq, taq = taq),
               ignore_attr = TRUE)

  Y$label <- LETTERS[1:15]
  Z <- as_count(Y)
  expect_equal(dim(Z), dim(X))

  Y$logic <- rep(c(TRUE, FALSE, TRUE), 5)
  A <- as_incidence(Y)
  expect_equal(dim(A), dim(X) + c(0, 1))

  Y <- data.frame(samples = character(0), groups = character(0),
                  dates = integer(0), tpq = integer(0), taq = integer(0))
  expect_error(as_count(Y))
})
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
test_that("CountMatrix <> IncidenceMatrix", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)

  counts1 <- as_count(cts)
  expect_s4_class(as_incidence(counts1), "IncidenceMatrix")
})
test_that("DataMatrix > long", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  counts <- as(cts, "CountMatrix")

  A <- as_long(counts, factor = TRUE, reverse = FALSE)
  expect_equal(dim(A), c(50, 8))
  expect_s3_class(A$row, "factor")
  expect_s3_class(A$column, "factor")
  expect_type(A$dates, "integer")
  expect_type(A$tpq, "integer")
  expect_type(A$taq, "integer")

  B <- as_long(counts, factor = TRUE, reverse = TRUE)
  expect_equal(rev(levels(A$row)), levels(B$row))
})
test_that("DataMatrix > features", {
  cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
  freq <- as_composition(cts)
  feat <- as_features(freq)

  expect_equal(dim(feat), c(5, 15))
})
