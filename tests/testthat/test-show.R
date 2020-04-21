context("Show")

test_that("Logical matrix", {
  B <- .IncidenceMatrix()
  expect_output(show(B), "IncidenceMatrix")

  C <- .StratigraphicMatrix()
  expect_output(show(C), "StratigraphicMatrix")
})
test_that("Integer matrix", {
  B <- .CountMatrix()
  expect_output(show(B), "CountMatrix")
})
test_that("Numeric matrix", {
  C <- .AbundanceMatrix()
  expect_output(show(C), "AbundanceMatrix")

  D <- .SimilarityMatrix()
  expect_output(show(D), "SimilarityMatrix")

  E <- .OccurrenceMatrix()
  expect_output(show(E), "OccurrenceMatrix")
})
