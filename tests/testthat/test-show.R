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
test_that("Extra info", {
  with_seed(12345, {
    X <- matrix(data = sample(0:10, 75, TRUE), nrow = 15, ncol = 5)
    Y <- as.data.frame(X)
    Y$samples <- rep(c("a", "b", "c", "d", "e"), each = 3)
    Y$groups <- rep(c("A", "B", "C"), each = 5)
    Y$tpq <- sample(1301:1400, 15, TRUE) # TPQ
    Y$taq <- sample(1451:1500, 15, TRUE) # TAQ
    Z <- as_count(Y)

    expect_snapshot(show(Z))
  })
})
