test_that("Wide to long", {
  ## Create a matrix
  A <- matrix(data = c(0, 9, 6, 9, 0, 0, 4, 0, 9, 1, 4, 1, 4, 6, 7),
              nrow = 5, ncol = 3)

  ## Transform to long data.frame
  expect_snapshot(wide_to_long(A))
})
