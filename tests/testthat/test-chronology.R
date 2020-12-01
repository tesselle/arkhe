context("Chronology")

mtx <- CountMatrix(data = sample(0:10, 100, TRUE), ncol = 10)

test_that("dates can be set with a numeric or integer vector", {
  k <- seq_len(10)
  set_dates(mtx) <- k
  dates <- get_dates(mtx)

  expect_type(dates, "list")
  expect_equal(dim(dates), c(10, 2))
  expect_equal(dates[, 1], seq_len(10))
  expect_equal(dates[, 2], rep(NA_real_, times = 10))

  expect_error(set_dates(mtx) <- seq_len(20), "Cannot interpret")
})
test_that("dates can be set with a character vector", {
  k <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  set_dates(mtx) <- k
  dates <- get_dates(mtx)

  expect_type(dates, "list")
  expect_equal(dim(dates), c(10, 2))
  expect_equal(dates[, 1], seq_len(10))
  expect_equal(dates[, 2], rep(NA_real_, times = 10))

  expect_error(set_dates(mtx) <- LETTERS, "Incorrect roman number.")
})
test_that("dates can be set with a list", {
  k <- list(value = seq_len(10), error = seq_len(10))
  set_dates(mtx) <- k
  dates <- get_dates(mtx)

  expect_type(dates, "list")
  expect_equal(dim(dates), c(10, 2))
  expect_equal(dates[, 1], seq_len(10))
  expect_equal(dates[, 2], seq_len(10))

  k <- list(value = seq_len(10))
  expect_error(set_dates(mtx) <- k, "does not have components")
})
test_that("dates can be set with a matrix", {
  k <- matrix(data = seq_len(20), nrow = 10)
  set_dates(mtx) <- k
  dates <- get_dates(mtx)

  expect_type(dates, "list")
  expect_equal(dim(dates), c(10, 2))

  k <- matrix(data = sample(1:10, 10, TRUE), nrow = 10)
  expect_error(set_dates(mtx) <- k, "must have at least 2 columns")
})
test_that("dates can be set with a data.frame", {
  k <- data.frame(value = seq_len(10), error = seq_len(10))
  set_dates(mtx) <- k
  dates <- get_dates(mtx)

  expect_type(dates, "list")
  expect_equal(dim(dates), c(10, 2))

  k <- data.frame(value = seq_len(10))
  expect_error(set_dates(mtx) <- k, "must have at least 2 columns")
})
test_that("dates can be matched by names", {
  k <- data.frame(value = seq_len(10), error = seq_len(10))
  rownames(k) <- paste0("row", 10:1)
  set_dates(mtx) <- k
  dates <- get_dates(mtx)

  expect_type(dates, "list")
  expect_equal(dim(dates), c(10, 2))
  expect_equal(dates[, 1], 10:1)
  expect_equal(dates[, 2], 10:1)
})
test_that("dates can be set to a Matrix", {
  options(arkhe.verbose = TRUE)
  X <- CountMatrix(data = sample(1:10, 100, TRUE), nrow = 10)

  Y <- sample(1:10, 10, TRUE)
  expect_message(set_dates(X) <- Y, "Errors are missing, NA generated.")

  Y <- matrix(data = sample(1:10, 20, TRUE), nrow = 10)
  expect_message(set_dates(X) <- Y, "10 dates were set.")

  Z <- Y[1:5, ]
  rownames(Z) <- LETTERS[seq_len(5)]
  expect_message(suppressWarnings(set_dates(X) <- Z), "0 dates were set.")
  expect_warning(suppressMessages(set_dates(X) <- Z), "do not match")

  set_dates(X) <- NULL
  expect_equal(dim(get_dates(X)), c(0, 2))
})
