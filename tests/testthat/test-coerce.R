context("Coerce")

# matrix =======================================================================
mtx_count <- matrix(sample(1:100, 50, TRUE), ncol = 10)
mtx_freq <- mtx_count / rowSums(mtx_count)
# Begin workaround: prevent row sums to zero
mtx_incid <- matrix(sample(0:1, 50, TRUE), ncol = 10)
mtx_incid[which(rowSums(mtx_incid) == 0), 1] <- 1
mtx_incid <- matrix(as.logical(mtx_incid), ncol = 10)
# End workaround
mtx_sim <- matrix(1, nrow = 5, ncol = 5)

test_that("matrix > CountMatrix", {
  A <- as_count(mtx_count)
  expect_s4_class(A, "CountMatrix")
  expect_equivalent(as(A, "matrix"), mtx_count)
})
test_that("matrix > AbundanceMatrix", {
  B <- as_abundance(mtx_freq)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equivalent(as(B, "matrix"), mtx_freq)

  expect_s4_class(as_abundance(mtx_count), "AbundanceMatrix")
  expect_s4_class(as_abundance(mtx_incid), "AbundanceMatrix")
})
test_that("matrix > IncidenceMatrix", {
  C <- as_incidence(mtx_incid)
  expect_s4_class(C, "IncidenceMatrix")
  expect_equivalent(as(C, "matrix"), mtx_incid)

  expect_s4_class(as_incidence(mtx_count), "IncidenceMatrix")
  expect_s4_class(as_incidence(mtx_freq), "IncidenceMatrix")
})
test_that("matrix > OccurrenceMatrix", {
  D <- as_occurrence(mtx_incid)
  expect_s4_class(D, "OccurrenceMatrix")
  expect_is(as(D, "matrix"), "matrix")

  expect_s4_class(as_occurrence(mtx_count), "OccurrenceMatrix")
  expect_s4_class(as_occurrence(mtx_freq), "OccurrenceMatrix")
})
test_that("matrix > SimilarityMatrix", {
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
  B <- as_abundance(df_freq)
  expect_s4_class(B, "AbundanceMatrix")
  expect_equivalent(as(B, "data.frame"), df_freq)

  expect_s4_class(as_abundance(df_count), "AbundanceMatrix")
  expect_s4_class(as_abundance(df_incid), "AbundanceMatrix")
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
  freq1 <- as_abundance(count1)
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
  expect_s4_class(as_abundance(count), "AbundanceMatrix")
  expect_s4_class(as_abundance(freq), "AbundanceMatrix")
  expect_s4_class(as_abundance(incid), "AbundanceMatrix")
  # expect_error(as_abundance(occ))
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
test_that("*Matrix > features", {
  expect_message(as_features(count), "No coordinates were set, NA generated.")
  expect_message(as_features(count), "No dates were set, NA generated.")
})
