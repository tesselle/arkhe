context("Extract")

test_that("SpaceTime", {
  space_time <- new("SpaceTime")
  expect_type(space_time[["dates"]], "list")
  expect_type(space_time[["coordinates"]], "list")
})
test_that("AbundanceMatrix", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)
  # ID
  expect_type(get_id(A1), "character")
})
test_that("NumericMatrix", {
  mtx_count <- matrix(sample(1:100, 100, TRUE), ncol = 10,
                      dimnames = list(LETTERS[1:10], LETTERS[26:17]))
  freq <- .FrequencyMatrix(mtx_count / rowSums(mtx_count),
                           totals = rowSums(mtx_count))

  expect_equal(get_totals(freq), rowSums(mtx_count))
  set_totals(freq) <- seq_len(10)
  expect_equal(get_totals(freq), seq_len(10))
})
