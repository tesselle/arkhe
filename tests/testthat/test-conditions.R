test_that("Error", {
  X <- catch_conditions(throw_error("custom_error", "E"))
  expect_s3_class(X[[1]], c("custom_error", "error", "condition"))

  X <- catch_message(stop("E"))
  expect_equal(X, "E")
})
test_that("Warning", {
  X <- catch_conditions(throw_warning("custom_warning", "W"))
  expect_s3_class(X[[1]], c("custom_warning", "warning", "condition"))

  X <- catch_message(warning("W"))
  expect_equal(X, "W")
})
test_that("Message", {
  X <- catch_conditions(message("M"))
  expect_s3_class(X[[1]], c("simpleMessage", "message", "condition"))

  X <- catch_message(message("M"))
  expect_equal(X, "M\n")
})
