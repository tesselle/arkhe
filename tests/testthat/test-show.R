test_that("Integer matrix", {
  A <- .CountMatrix()
  expect_output(show(A), "CountMatrix")

  B <- .OccurrenceMatrix()
  expect_output(show(B), "OccurrenceMatrix")
})
test_that("Numeric matrix", {
  A <- .CompositionMatrix()
  expect_output(show(A), "CompositionMatrix")
})
test_that("Logical matrix", {
  A <- .IncidenceMatrix()
  expect_output(show(A), "IncidenceMatrix")

  B <- .StratigraphicMatrix()
  expect_output(show(B), "StratigraphicMatrix")
})
