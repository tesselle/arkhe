context("Chronology")

test_that("AbundanceMatrix - Dates", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(1:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)

  # Time coordinates
  dates <- list(value = seq_len(10), error = rep(0, 10))
  roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  num <- seq_len(10)

  expect_message(set_dates(A1) <- roman)
  expect_equal(get_dates(A1), dates)

  expect_message(set_dates(A1) <- num)
  expect_equal(get_dates(A1), dates)

  expect_message(set_dates(A1) <- dates)
  expect_equal(get_dates(A1), dates)

  expect_message(set_dates(A1) <- as.data.frame(dates))
  expect_equal(get_dates(A1), dates)

  expect_message(set_dates(A1) <- as.data.frame(unname(dates)))
  expect_equal(get_dates(A1), dates)

  expect_error(set_dates(A1) <- as.data.frame(dates[-2]),
               "should have at least 2 columns")
  expect_error(set_dates(A1) <- dates[1], "does not have components")
  expect_error(set_dates(A1) <- NA,
               "a list, a matrix or a data frame is expected")
  expect_error(set_dates(A1) <- "X", "Cannot interpret")
  expect_warning(set_dates(A1) <- rep("A", 10))
})
