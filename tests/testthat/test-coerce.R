context("Coerce")

# matrix =======================================================================
cts <- matrix(sample(0:100, 50, TRUE), ncol = 10)
freq <- prop.table(cts, 1)
incid <- matrix(as.logical(sample(0:1, 50, TRUE)), ncol = 10)
sim <- matrix(1, nrow = 5, ncol = 5)

test_that("matrix > CountMatrix", {
  A <- as_count(cts)

  expect_s4_class(A, "CountMatrix")
  expect_equivalent(as.matrix(A), cts)
})
test_that("matrix > AbundanceMatrix", {
  B <- as_abundance(cts)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equivalent(as.matrix(B), freq)

  expect_s4_class(as_abundance(incid), "AbundanceMatrix")
})
test_that("matrix > IncidenceMatrix", {
  C <- as_incidence(incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equivalent(as.matrix(C), incid)

  expect_s4_class(as_incidence(cts), "IncidenceMatrix")
  expect_s4_class(as_incidence(freq), "IncidenceMatrix")
})
test_that("matrix > OccurrenceMatrix", {
  D <- as_occurrence(cts)
  expect_s4_class(D, "OccurrenceMatrix")

  expect_s4_class(as_occurrence(cts), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(freq), "OccurrenceMatrix")
})
test_that("matrix > SimilarityMatrix", {
  E <- as_similarity(sim)
  expect_s4_class(E, "SimilarityMatrix")
})

# data.frame ===================================================================
df_count <- as.data.frame(cts)
df_freq <- as.data.frame(freq)
df_incid <- as.data.frame(incid)
df_sim <- as.data.frame(sim)

test_that("data.frame <> CountMatrix", {
  A <- as_count(df_count)
  expect_s4_class(A, "CountMatrix")
  expect_equivalent(as.data.frame(A), df_count)
})
test_that("data.frame <> AbundanceMatrix", {
  B <- as_abundance(df_freq)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equivalent(as.data.frame(B), df_freq)

  expect_s4_class(as_abundance(df_count), "AbundanceMatrix")
  expect_s4_class(as_abundance(df_incid), "AbundanceMatrix")
})
test_that("data.frame <> IncidenceMatrix", {
  C <- as_incidence(df_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equivalent(as.data.frame(C), df_incid)

  expect_s4_class(as_incidence(df_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(df_freq), "IncidenceMatrix")
})
test_that("data.frame <> OccurrenceMatrix", {
  D <- as_occurrence(df_incid)
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as.data.frame(D), "data.frame")

  expect_s4_class(as_occurrence(df_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(df_freq), "OccurrenceMatrix")
})
test_that("data.frame <> SimilarityMatrix", {
  E <- as_similarity(df_sim)
  expect_s4_class(E, "SimilarityMatrix")
  expect_is(as.data.frame(E), "data.frame")
})

# *Matrix ======================================================================
count <- as(cts, "CountMatrix")
freq <- as(cts, "AbundanceMatrix")
incid <- as(incid, "IncidenceMatrix")
occ <- as(incid, "OccurrenceMatrix")

test_that("CountMatrix <> AbundanceMatrix", {
  count1 <- as_count(cts)
  freq1 <- as_abundance(count1)
  count2 <- as_count(freq1)
  expect_equal(count1, count2)
})
test_that("DataMatrix > CountMatrix", {
  expect_s4_class(as_count(count), "CountMatrix")
})
test_that("DataMatrix > AbundanceMatrix", {
  expect_s4_class(as_abundance(count), "AbundanceMatrix")
  expect_s4_class(as_abundance(freq), "AbundanceMatrix")
})
test_that("DataMatrix > IncidenceMatrix", {
  expect_s4_class(as_incidence(count), "IncidenceMatrix")
  expect_s4_class(as_incidence(freq), "IncidenceMatrix")
  expect_s4_class(as_incidence(incid), "IncidenceMatrix")
})
test_that("DataMatrix > OccurrenceMatrix", {
  expect_s4_class(as_occurrence(count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(freq), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(incid), "OccurrenceMatrix")
})
test_that("DataMatrix > matrix", {
  expect_is(as(count, "matrix"), "matrix")
  expect_is(as(freq, "matrix"), "matrix")
  expect_is(as(incid, "matrix"), "matrix")
  expect_is(as(occ, "matrix"), "matrix")
})
test_that("matrix > long", {
  expect_equal(dim(as_long(cts)), c(50, 3))

  expect_s3_class(as_long(cts, factor = TRUE)$case, "factor")
  expect_s3_class(as_long(cts, factor = TRUE)$type, "factor")
})
test_that("DataMatrix > long", {
  expect_equal(dim(as_long(count)), c(50, 3))
  expect_equal(dim(as_long(freq)), c(50, 3))
  expect_equal(dim(as_long(incid)), c(50, 3))
  expect_equal(dim(as_long(occ)), c(100, 3))

  expect_s3_class(as_long(occ, factor = TRUE)$case, "factor")
  expect_s3_class(as_long(occ, factor = TRUE)$type, "factor")
})
test_that("DataMatrix > features", {
  expect_equal(dim(as_features(freq)), c(5, 12))
})
