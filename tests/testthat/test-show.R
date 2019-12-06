context("Show")

test_that("Logical matrix", {
  A <- methods::new("LogicalMatrix")
  expect_output(show(A), "An object of class \"LogicalMatrix\"")

  B <- IncidenceMatrix(matrix(TRUE))
  expect_output(show(B), "presence/absence data matrix")
})
test_that("Numeric matrix", {
  A <- methods::new("NumericMatrix")
  expect_output(show(A), "An object of class \"NumericMatrix\"")

  B <- CountMatrix(matrix(1))
  expect_output(show(B), "absolute frequency matrix")

  C <- AbundanceMatrix(matrix(1))
  expect_output(show(C), "relative frequency matrix")

  D <- .SimilarityMatrix(matrix(1))
  expect_output(show(D), "similarity matrix")

  E <- .OccurrenceMatrix(matrix(1))
  expect_output(show(E), "co-occurrence matrix")
})
