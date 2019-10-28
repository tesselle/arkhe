context("Show")

test_that("Logical matrix", {
  A <- methods::new("LogicalMatrix")
  expect_output(show(A), "An object of class \"LogicalMatrix\"")

  B <- methods::new("IncidenceMatrix")
  expect_output(show(B), "presence/absence data matrix")

  C <- methods::new("OccurrenceMatrix")
  expect_output(show(C), "co-occurrence matrix")
})
test_that("Numeric matrix", {
  A <- methods::new("NumericMatrix")
  expect_output(show(A), "An object of class \"NumericMatrix\"")

  B <- methods::new("CountMatrix")
  expect_output(show(B), "count data matrix")

  C <- methods::new("FrequencyMatrix")
  expect_output(show(C), "frequency data matrix")

  D <- methods::new("SimilarityMatrix")
  expect_output(show(D), "similarity matrix")
})
