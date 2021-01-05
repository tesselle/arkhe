context("Correspondence Analysis")

test_that("Predict new principal coordinates", {
  cts <- CountMatrix(data = sample(1:10, 100, TRUE), ncol = 10)

  res <- ca(cts[1:7, 1:6])
  new_rows <- predict(res, cts[8:10, 1:6], margin = 1)
  new_cols <- predict(res, cts[1:7, 7:10], margin = 2)

  res_sup <- ca(cts, sup_rows = 8:10, sup_columns = 7:10)
  rows <- get_coordinates(res_sup, margin = 1, standard = FALSE, sup = TRUE)
  cols <- get_coordinates(res_sup, margin = 2, standard = FALSE, sup = TRUE)

  expect_equal(new_rows, rows[rows$.sup, 1:5])
  expect_equal(new_cols, cols[cols$.sup, 1:5])
})
test_that("Compare with ca package", {
  skip_on_cran()
  skip_if_not_installed("ca")

  mtx <- matrix(data = sample(1:10, 100, TRUE), ncol = 10)
  cts <- as_count(mtx)

  res_ca <- ca::ca(mtx, suprow = 8:10, supcol = 7:10)
  res_arkhe <- ca(cts, sup_rows = 8:10, sup_columns = 7:10)

  # Row standard coordinates
  expect_equivalent(res_ca$rowcoord, res_arkhe@row_coordinates)
  # Column standard coordinates
  expect_equivalent(res_ca$colcoord, res_arkhe@column_coordinates)
  # Row distances
  expect_equivalent(res_ca$rowdist, res_arkhe@row_distances)
  # Column distances
  expect_equivalent(res_ca$coldist, res_arkhe@column_distances)
  # Row inertias
  expect_equivalent(res_ca$rowinertia, res_arkhe@row_inertias)
  # Column inertias
  expect_equivalent(res_ca$colinertia, res_arkhe@column_inertias)
})
