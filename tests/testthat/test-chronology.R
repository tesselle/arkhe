context("Chronology")

test_that("dates can be set with a matrix or data.frame", {
  X <- matrix(data = sample(1:10, 20, TRUE), nrow = 10)
  expect_type(make_dates(X), "integer")

  X <- matrix(data = sample(1:10, 20, TRUE), nrow = 10,
              dimnames = list(NULL, c("value", "error")))
  expect_type(make_dates(X), "integer")

  X <- matrix(data = sample(1:10, 10, TRUE), nrow = 10)
  expect_error(make_dates(X), "must have at least 2 columns")
})
test_that("dates can be set with a list", {
  X <- list(value = seq_len(10), error = rep(0, 10))
  expect_type(make_dates(X), "double")

  X <- list(value = seq_len(10))
  expect_error(make_dates(X), "does not have components")
})
# test_that("dates can be set with a numeric or integer vector", {
#   X <- seq_len(10)
#   expect_message(make_dates(X), "Errors are missing, NA generated.")
# })
test_that("dates can be set with a character vector", {
  roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  X <- make_dates(roman)
  expect_type(X, "double")
  expect_equal(dim(X), c(10, 2))
  expect_equal(X[, 1], seq_len(10))
  expect_equal(X[, 2], rep(NA_real_, times = 10))

  expect_error(make_dates(LETTERS))
})
test_that("dates cannot be set with garbage", {
  expect_type(make_dates(NULL), "double")
  expect_error(make_dates(NA), "a list, a matrix or a data frame is expected")
})
test_that("dates can be set to a Matrix", {
  options("verbose" = TRUE)
  X <- CountMatrix(data = sample(1:10, 100, TRUE), nrow = 10)

  Y <- sample(1:10, 10, TRUE)
  expect_message(set_dates(X) <- Y, "Errors are missing, NA generated.")

  Y <- matrix(data = sample(1:10, 20, TRUE), nrow = 10)
  expect_message(set_dates(X) <- Y, "10 dates were set.")
  expect_type(get_dates(X), "list")
  expect_equivalent(as.matrix(get_dates(X)), Y)

  Z <- Y[1:5, ]
  rownames(Z) <- LETTERS[seq_len(5)]
  expect_message(suppressWarnings(set_dates(X) <- Z), "0 dates were set.")
  expect_warning(suppressMessages(set_dates(X) <- Z), "do not match")

  expect_error(set_dates(X) <- "Y", "Incorrect roman number.")
  expect_error(set_dates(X) <- 1, "Cannot interpret `value` in a suitable way.")
})
test_that("dates can be get from a Matrix", {
  X <- CountMatrix(data = sample(1:10, 100, TRUE), nrow = 10)
  set_dates(X) <- NULL
  Y <- get_dates(X)

  expect_s3_class(Y, "data.frame")
  expect_equal(dim(Y), c(0, 2))
})
