context("Extract")

test_that("Matrix", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)
  # ID
  expect_type(get_id(A1), "character")
})
test_that("NumericMatrix", {
  mtx_count <- matrix(sample(1:100, 100, TRUE), ncol = 10,
                      dimnames = list(LETTERS[1:10], LETTERS[26:17]))
  freq <- .AbundanceMatrix(mtx_count / rowSums(mtx_count),
                           id = generate_uuid(),
                           totals = rowSums(mtx_count))

  expect_equal(get_totals(freq), rowSums(mtx_count))
  set_totals(freq) <- seq_len(10)
  expect_equal(get_totals(freq), seq_len(10))
})
