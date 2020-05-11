context("Getters and setters")

test_that("Matrix id", {
  id <- generate_uuid()
  mtx <- .CountMatrix()

  set_id(mtx) <- id
  expect_identical(get_id(mtx), id)
})
test_that("AbundanceMatrix totals", {
  count <- matrix(sample(1:100, 100, TRUE), ncol = 10)

  freq <- as_abundance(count)
  expect_identical(get_totals(freq), rowSums(count))

  set_totals(freq) <- seq_len(10)
  expect_identical(get_totals(freq), seq_len(10))
})
test_that("SimilarityMatrix method", {
  sim <- .SimilarityMatrix()
  expect_identical(get_method(sim), "unknown")

  set_method(sim) <- "xxx"
  expect_identical(get_method(sim), "xxx")
})
test_that("StratigraphicMatrix units", {
  strati <- .StratigraphicMatrix(data = FALSE, size = c(1L, 1L),
                                 row_names = "A", column_names = "A")
  expect_identical(get_units(strati), "A")
})
