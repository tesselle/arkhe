context("Show")

test_that("Logical matrix", {
  A <- methods::new("LogicalMatrix")
  expect_output(show(A), "An object of class \"LogicalMatrix\"")

  B <- .IncidenceMatrix()
  expect_output(show(B), "presence/absence data matrix")

  C <- .StratigraphicMatrix()
  expect_output(show(C), "stratigraphic matrix")
})
test_that("Numeric matrix", {
  A <- methods::new("NumericMatrix")
  expect_output(show(A), "An object of class \"NumericMatrix\"")

  B <- .CountMatrix()
  expect_output(show(B), "absolute frequency matrix")

  C <- .AbundanceMatrix()
  expect_output(show(C), "relative frequency matrix")

  D <- .SimilarityMatrix()
  expect_output(show(D), "similarity matrix")

  E <- .OccurrenceMatrix()
  expect_output(show(E), "co-occurrence matrix")
})
