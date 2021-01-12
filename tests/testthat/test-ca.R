context("Correspondence Analysis")

test_that("Predict new principal coordinates", {
  cts <- CountMatrix(data = sample(1:10, 100, TRUE), ncol = 5)

  row_zeros <- col_zeros <- cts
  row_zeros[1, ] <- 0
  expect_error(ca(row_zeros), "Empty rows detected.")
  col_zeros[, 1] <- 0
  expect_error(ca(col_zeros), "Empty columns detected.")

  expect_error(ca(cts, sup_rows = "row1"), "must be a numeric vector")
  expect_error(ca(cts, sup_col = "col1"), "must be a numeric vector")

  res <- ca(cts, n = 10)
  # Points coordinates
  coord_row <- get_coordinates(res, margin = 1, sup = TRUE)
  expect_equal(dim(coord_row), c(20L, 5L))
  coord_col <- get_coordinates(res, margin = 2, sup = TRUE)
  expect_equal(dim(coord_col), c(5L, 5L))
  # Distances
  dist_row <- get_distances(res, margin = 1)
  expect_length(dist_row, 20)
  dist_col <- get_distances(res, margin = 2)
  expect_length(dist_col, 5)
  # Inertias
  inertia_row <- get_inertia(res, margin = 1)
  expect_length(inertia_row, 20)
  inertia_col <- get_inertia(res, margin = 2)
  expect_length(inertia_col, 5)
  # Eigenvalues
  eig <- get_eigenvalues(res)
  expect_equal(dim(eig), c(4L, 3L))
})
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
