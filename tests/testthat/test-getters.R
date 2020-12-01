context("Getters and setters")

test_that("AbundanceMatrix totals", {
  count <- matrix(sample(1:100, 100, TRUE), ncol = 10)

  freq <- as_abundance(count)
  expect_equivalent(get_totals(freq), rowSums(count))

  set_totals(freq) <- seq_len(10)
  expect_equivalent(get_totals(freq), seq_len(10))
})
test_that("SimilarityMatrix method", {
  sim <- .SimilarityMatrix()

  set_method(sim) <- "xxx"
  expect_identical(get_method(sim), "xxx")
})
