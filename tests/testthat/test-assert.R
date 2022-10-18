test_that("Validate", {
  expect_null(validate(mean(c(1,2, 3))))
  expect_type(validate(mean(c("a", "b", "c"))), "character")
  expect_length(validate(mean(c("a", "b", "c"))), 1)
})
test_that("Assert type", {
  x <- numeric()
  cnd <- catch_conditions(assert_type(x, expected = "character"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  expect_identical(assert_type(x, expected = "numeric"), x)
})
test_that("Assert scalar", {
  x <- numeric(3)
  cnd <- catch_conditions(assert_scalar(x, expected = "character"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  y <- numeric(1)
  cnd <- catch_conditions(assert_scalar(y, expected = "character"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  expect_identical(assert_scalar(y, expected = "numeric"), y)
})
test_that("Assert object attributes", {
  ## Length
  cnd <- catch_conditions(assert_length(LETTERS, expected = 10, empty = FALSE))
  expect_s3_class(cnd[[1]], "error_bad_length")
  cnd <- catch_conditions(assert_length(LETTERS, expected = 10, empty = TRUE))
  expect_s3_class(cnd[[1]], "error_bad_length")
  cnd <- catch_conditions(assert_length(numeric(0), expected = 10, empty = FALSE))
  expect_s3_class(cnd[[1]], "error_bad_length")

  expect_invisible(assert_length(numeric(0), expected = 10, empty = TRUE))
  expect_identical(assert_length(LETTERS, expected = 26), LETTERS)

  ## Lengths
  k <- list(1:10, 1:20)
  cnd <- catch_conditions(assert_lengths(k, expected = c(20, 10)))
  expect_s3_class(cnd[[1]], "error_bad_length")

  expect_identical(assert_lengths(k, expected = c(10, 20)), k)

  ## Empty
  cnd <- catch_conditions(assert_empty(k))
  expect_s3_class(cnd[[1]], "error_bad_dimensions")

  k <- list()
  cnd <- catch_conditions(assert_filled(k))
  expect_s3_class(cnd[[1]], "error_bad_dimensions")
  expect_identical(assert_empty(k), k)

  ## Dimensions
  k <- matrix(1, nrow = 10, ncol = 5)
  cnd <- catch_conditions(assert_dimensions(k, expected = c(5, 10)))
  expect_s3_class(cnd[[1]], "error_bad_dimensions")

  expect_identical(assert_dimensions(k, expected = c(10, 5)), k)

  ## Names
  k <- vector(mode = "numeric", length = 10)
  cnd <- catch_conditions(assert_names(k, expected = NULL))
  expect_s3_class(cnd[[1]], "error_bad_names")

  cnd <- catch_conditions(assert_names(k, expected = LETTERS[11:20]))
  expect_s3_class(cnd[[1]], "error_bad_names")

  names(k) <- LETTERS[1:10]
  cnd <- catch_conditions(assert_names(k, expected = LETTERS[11:20]))
  expect_s3_class(cnd[[1]], "error_bad_names")

  expect_identical(assert_names(k, expected = LETTERS[1:10]), k)

  ## Dimnames
  k <- matrix(1, nrow = 3, ncol = 3)
  z <- list(LETTERS[1:3], LETTERS[4:6])
  cnd <- catch_conditions(assert_dimnames(k, expected = z))
  expect_s3_class(cnd[[1]], "error_bad_names")

  cnd <- catch_conditions(assert_rownames(k, expected = z[[1]]))
  expect_s3_class(cnd[[1]], "error_bad_names")

  cnd <- catch_conditions(assert_colnames(k, expected = z[[2]]))
  expect_s3_class(cnd[[1]], "error_bad_names")

  dimnames(k) <- list(NULL, letters[4:6])
  cnd <- catch_conditions(assert_dimnames(k, expected = z))
  expect_s3_class(cnd[[1]], "error_bad_names")

  dimnames(k) <- list(letters[1:3], letters[4:6])
  cnd <- catch_conditions(assert_dimnames(k, expected = z))
  expect_s3_class(cnd[[1]], "error_bad_names")

  dimnames(k) <- z
  expect_identical(assert_dimnames(k, expected = z), k)
})
test_that("Assert missing/infinite/duplicated values", {
  ## Missing
  k <- sample(c(1, NA), size = 15, replace = TRUE)
  cnd <- catch_conditions(assert_missing(k))
  expect_s3_class(cnd[[1]], "error_data_missing")

  k <- sample(c(1, NaN), size = 15, replace = TRUE)
  cnd <- catch_conditions(assert_missing(k))
  expect_s3_class(cnd[[1]], "error_data_missing")

  expect_identical(assert_missing(numeric(3)), numeric(3))

  ## Infinite
  k <- sample(c(1, Inf), size = 15, replace = TRUE)
  cnd <- catch_conditions(assert_infinite(k))
  expect_s3_class(cnd[[1]], "error_data_infinite")

  expect_identical(assert_infinite(numeric(3)), numeric(3))

  ## Duplicates
  k <- sample(c(1, 2), size = 15, replace = TRUE)
  cnd <- catch_conditions(assert_unique(k))
  expect_s3_class(cnd[[1]], "error_data_duplicates")

  expect_identical(assert_unique(c(1, 2, 3)), c(1, 2, 3))
})
test_that("Assert numeric data", {
  k <- seq(from = -1, to = 10, by = 1)
  cnd <- catch_conditions(assert_positive(k, strict = FALSE))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  k <- seq(from = 0, to = 10, by = 1)
  cnd <- catch_conditions(assert_positive(k, strict = TRUE))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  k <- seq(from = 1, to = 10, by = 0.5)
  cnd <- catch_conditions(assert_whole(k))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  k <- seq(from = 1, to = 10, by = 0.5)
  cnd <- catch_conditions(assert_whole(k, tolerance = 0.2))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  k <- c(2, 4, 6, 8, 10)
  cnd <- catch_conditions(assert_odd(k))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  k <- c(1, 3, 5, 7, 9)
  cnd <- catch_conditions(assert_even(k))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  expect_identical(assert_positive(k), k)
})
test_that("Assert count data", {
  x <- c(1.1, 2, 3, 4, 5)
  cnd <- catch_conditions(assert_count(x))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  x <- c(1, 2, 3, 4, 5)
  expect_equal(assert_count(x), x)
})
test_that("Assert numeric trends", {
  k <- c(1, 3, 5, 7, 9)
  cnd <- catch_conditions(assert_constant(k))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  cnd <- catch_conditions(assert_decreasing(k))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  cnd <- catch_conditions(assert_increasing(rev(k)))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  expect_identical(assert_increasing(k), k)
})
test_that("Assert numeric relation", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 6, 7, 8, 9)

  cnd <- catch_conditions(assert_greater(x, y))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  cnd <- catch_conditions(assert_lower(y, x))
  expect_s3_class(cnd[[1]], "error_bad_numeric")

  expect_identical(assert_lower(x, y), x)
})
test_that("Assert matrix", {
  k <- matrix(sample(1:5, 50, TRUE), nrow = 5, ncol = 10)

  cnd <- catch_conditions(assert_square(k))
  expect_s3_class(cnd[[1]], "error_bad_matrix")

  cnd <- catch_conditions(assert_symmetric(k))
  expect_s3_class(cnd[[1]], "error_bad_matrix")

  k <- matrix(sample(1:5, 25, TRUE), nrow = 5, ncol = 5)
  expect_identical(assert_square(k), k)
})
