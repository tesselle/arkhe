context("Coerce")

# matrix =======================================================================
mtx_count <- matrix(sample(0:10, 50, TRUE), ncol = 10)
mtx_freq <- mtx_count / rowSums(mtx_count)
mtx_incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)
mtx_sim <- matrix(1, nrow = 5, ncol = 5)

test_that("matrix objects can be coerced to CountMatrix", {
  A <- as_count(mtx_count)
  expect_s4_class(A, "CountMatrix")
  expect_equivalent(as(A, "matrix"), mtx_count)
})
test_that("matrix objects can be coerced to AbundanceMatrix", {
  B <- as_frequency(mtx_freq)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equivalent(as(B, "matrix"), mtx_freq)

  expect_s4_class(as_frequency(mtx_count), "AbundanceMatrix")
  expect_s4_class(as_frequency(mtx_incid), "AbundanceMatrix")
})
test_that("matrix objects can be coerced to IncidenceMatrix", {
  C <- as_incidence(mtx_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equivalent(as(C, "matrix"), mtx_incid)

  expect_s4_class(as_incidence(mtx_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(mtx_freq), "IncidenceMatrix")
})
test_that("matrix objects can be coerced to OccurrenceMatrix", {
  D <- as_occurrence(mtx_incid)
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "matrix"), "matrix")

  expect_s4_class(as_occurrence(mtx_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(mtx_freq), "OccurrenceMatrix")
})
test_that("matrix objects can be coerced to SimilarityMatrix", {
  E <- as_similarity(mtx_sim)
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as(E, "matrix"), "matrix")
})

# data.frame ===================================================================
df_count <- as.data.frame(mtx_count)
df_freq <- as.data.frame(mtx_freq)
df_incid <- as.data.frame(mtx_incid)
df_sim <- as.data.frame(mtx_sim)

test_that("data.frame <> CountMatrix", {
  A <- as_count(df_count)
  expect_s4_class(A, "CountMatrix")
  expect_equivalent(as(A, "data.frame"), df_count)
})
test_that("data.frame <> AbundanceMatrix", {
  B <- as_frequency(df_freq)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equivalent(as(B, "data.frame"), df_freq)

  expect_s4_class(as_frequency(df_count), "AbundanceMatrix")
  expect_s4_class(as_frequency(df_incid), "AbundanceMatrix")
})
test_that("data.frame <> IncidenceMatrix", {
  C <- as_incidence(df_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equivalent(as(C, "data.frame"), df_incid)

  expect_s4_class(as_incidence(df_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(df_freq), "IncidenceMatrix")
})
test_that("data.frame <> OccurrenceMatrix", {
  D <- as_occurrence(df_incid)
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "data.frame"), "data.frame")

  expect_s4_class(as_occurrence(df_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_freq), "OccurrenceMatrix")
})
test_that("data.frame <> SimilarityMatrix", {
  E <- as_similarity(df_sim)
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as(E, "data.frame"), "data.frame")
})

# *Matrix ======================================================================
count <- as(mtx_count, "CountMatrix")
freq <- as(mtx_freq, "AbundanceMatrix")
incid <- as(mtx_incid, "IncidenceMatrix")
occ <- as(mtx_incid, "OccurrenceMatrix")

test_that("CountMatrix <> AbundanceMatrix", {
  count1 <- as_count(mtx_count)
  freq1 <- as_frequency(count1)
  count2 <- as_count(freq1)
  expect_identical(count1, count2)

  freq1@totals <- numeric(0)
  expect_error(as_count(freq1), "Cannot calculate absolute frequencies")
})
test_that("*Matrix > CountMatrix", {
  expect_s4_class(as_count(count), "CountMatrix")
  # expect_error(as_count(occ))
})
test_that("*Matrix > AbundanceMatrix", {
  expect_s4_class(as_frequency(count), "AbundanceMatrix")
  expect_s4_class(as_frequency(freq), "AbundanceMatrix")
  expect_s4_class(as_frequency(incid), "AbundanceMatrix")
  # expect_error(as_frequency(occ))
})
test_that("*Matrix > IncidenceMatrix", {
  expect_s4_class(as_incidence(count), "IncidenceMatrix")
  expect_s4_class(as_incidence(freq), "IncidenceMatrix")
  expect_s4_class(as_incidence(incid), "IncidenceMatrix")
  # expect_error(as_incidence(occ))
})
test_that("*Matrix > OccurrenceMatrix", {
  expect_s4_class(as_occurrence(count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(freq), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(incid), "OccurrenceMatrix")
})
