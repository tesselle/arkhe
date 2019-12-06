context("Conditions")

test_that("Error", {
  X <- catch_conditions(throw_error("custom_error", "E"))
  expect_equal(class(X[[1]]), c("custom_error", "error", "condition"))

  X <- catch_conditions(stop("E"))
  expect_equivalent(X[[1]][1], c("E"))
  expect_equal(class(X[[1]]), c("simpleError", "error", "condition"))
})

test_that("Warning", {
  X <- catch_conditions(throw_warning("custom_warning", "W"))
  expect_equal(class(X[[1]]), c("custom_warning", "warning", "condition"))

  X <- catch_conditions(warning("W"))
  expect_equivalent(X[[1]][1], c("W"))
  expect_equal(class(X[[1]]), c("simpleWarning", "warning", "condition"))
})

test_that("Message", {
  X <- catch_conditions(message("M"))
  expect_equivalent(X[[1]][1], c("M\n"))
  expect_equal(class(X[[1]]), c("simpleMessage", "message", "condition"))
})
