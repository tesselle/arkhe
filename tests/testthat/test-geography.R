context("Geography")

test_that("AbundanceMatrix - Coordinates", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)

  # Geographic coordinates
  set_epsg(A1) <- 12345
  expect_equal(get_epsg(A1), 12345)
  expect_equal(attr(get_coordinates(A1), "epsg"), 12345)
  expect_error(set_epsg(A1) <- "X")

  coords <- list(X = sample(0:10, 10, TRUE),
                 Y = sample(0:10, 10, TRUE),
                 Z = sample(0:10, 10, TRUE))
  expect_message(set_coordinates(A1) <- coords[-3])
  expect_equal(get_coordinates(A1)[-3], coords[-3])
  expect_equal(get_coordinates(A1)[[3]], rep(NA_real_, 10))
  expect_silent(set_coordinates(A1) <- coords)
  expect_equal(get_coordinates(A1)[seq_len(3)], coords)
  expect_silent(set_coordinates(A1) <- as.data.frame(coords))
  expect_equal(get_coordinates(A1)[seq_len(3)], coords)
  expect_silent(set_coordinates(A1) <- as.data.frame(unname(coords)))
  expect_equal(get_coordinates(A1)[seq_len(3)], coords)
  expect_message(set_coordinates(A1) <- as.data.frame(coords[-3]))
  expect_equal(get_coordinates(A1)[-3], coords[-3])

  expect_error(set_coordinates(A1) <- as.data.frame(coords[-c(1, 2)]),
               "should have at least 2 columns")
  expect_error(set_coordinates(A1) <- coords[1], "does not have components")
  expect_error(set_coordinates(A1) <- rep("A", 10),
               "A list, a matrix or a data frame is expected")
})
