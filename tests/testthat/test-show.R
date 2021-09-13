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
    Z <- as_count(Y)

    S <- summary(Z)
    expect_snapshot(show(S))

    set_samples(Z) <- sample(c("a", "b", "c", "d", "e"), 15, TRUE)
    set_groups(Z) <- sample(c("A", "B", "C", "D", "E"), 15, TRUE)
    set_dates(Z) <- sample(1400:1451, 15, TRUE)
    set_tpq(Z) <- sample(1301:1400, 15, TRUE)
    set_taq(Z) <- sample(1451:1500, 15, TRUE)

    S <- summary(Z)
    expect_snapshot(show(S))
  })
})
