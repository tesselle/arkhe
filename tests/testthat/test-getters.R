context("Get")

test_that("Matrix - get_id", {
  id <- generate_uuid()
  mtx <- .Matrix(1, id = id)
  expect_equal(get_id(mtx), id)
})
test_that("AbundanceMatrix - get_totals", {
  count <- matrix(sample(1:100, 100, TRUE), ncol = 10)

  freq <- .AbundanceMatrix(count / rowSums(count), totals = rowSums(count))
  expect_equal(get_totals(freq), rowSums(count))

  set_totals(freq) <- seq_len(10)
  expect_equal(get_totals(freq), seq_len(10))
})
test_that("SimilarityMatrix - get_method", {
  sim <- .SimilarityMatrix()
  expect_equal(get_method(sim), "unknown")
})
test_that("StratigraphicMatrix - get_units", {
  strati <- .StratigraphicMatrix(FALSE, units = "A")
  expect_equal(get_units(strati), "A")
})
