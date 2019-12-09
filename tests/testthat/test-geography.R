context("Geography")

test_that("coordinates can be set with a matrix or data.frame", {
  X <- matrix(data = sample(1:10, 30, TRUE), nrow = 10)
  expect_type(make_coordinates(X), "integer")

  X <- matrix(data = sample(1:10, 30, TRUE), nrow = 10,
              dimnames = list(NULL, c("X", "Y", "Z")))
  expect_type(make_coordinates(X), "integer")

  X <- matrix(data = sample(1:10, 10, TRUE), nrow = 10)
  expect_error(make_coordinates(X), "must have at least 2 columns")
})
test_that("coordinates can be set with a list", {
  X <- list(X = seq_len(10), Y = rep(0, 10))
  expect_type(make_coordinates(X), "double")

  X <- list(X = seq_len(10), Y = rep(0, 10), Z = rep(0, 10))
  expect_type(make_coordinates(X), "double")

  X <- list(X = seq_len(10))
  expect_error(make_coordinates(X), "does not have components")
})
test_that("coordinates cannot be set with garbage", {
  expect_type(make_coordinates(NULL), "double")
  expect_error(make_coordinates(NA), "A list, a matrix or a data frame is expected.")
  expect_error(make_coordinates("Y"), "A list, a matrix or a data frame is expected.")
})
test_that("coordinates can be set to a Matrix", {
  options("verbose" = TRUE)
  X <- CountMatrix(sample(1:10, 100, TRUE), nrow = 10)

  Y <- matrix(sample(1:10, 20, TRUE), nrow = 10)
  expect_message(set_coordinates(X) <- Y, "'Z' is missing, NA generated.")

  Y <- matrix(sample(1:10, 30, TRUE), nrow = 10)
  expect_message(set_coordinates(X) <- Y, "10 coordinates were set.")
  expect_s3_class(get_coordinates(X), "data.frame")
  expect_equivalent(as.matrix(get_coordinates(X)), Y)

  Z <- Y[1:5, ]
  rownames(Z) <- LETTERS[seq_len(5)]
  expect_message(suppressWarnings(set_coordinates(X) <- Z), "0 coordinates were set.")
  expect_warning(suppressMessages(set_coordinates(X) <- Z), "do not match")

  expect_error(set_coordinates(X) <- matrix(c(1, 3, 3), ncol = 3), "Cannot interpret `value` in a suitable way.")
})
test_that("coordinates can be get from a Matrix", {
  X <- CountMatrix(data = sample(1:10, 100, TRUE), nrow = 10)

  set_coordinates(X) <- NULL
  Y <- get_coordinates(X)

  expect_s3_class(Y, "data.frame")
  expect_equal(dim(Y), c(0, 3))
})
