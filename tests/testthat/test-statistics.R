test_that("Highest density regions", {
  expect_snapshot(interval_hdr(faithful$eruptions))
})
test_that("Bayesian credible interval", {
  expect_snapshot(interval_credible(faithful$eruptions))
})
test_that("Confidence interval for the mean", {
  x <- c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,
         16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5, 15.5,
         15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8, 19.7, 15, 21.4)

  expect_snapshot(confidence_mean(x, type = "student"))
  expect_snapshot(confidence_mean(x, type = "normal"))
})
test_that("Confidence interval for binomial proportions", {
  expect_snapshot(confidence_binomial(118, n = 236))
})
test_that("Confidence interval for binomial proportions", {
  x <- c(35, 74, 22, 69)

  expect_snapshot(confidence_multinomial(x))
  expect_snapshot(confidence_multinomial(x, corrected = TRUE))
})
test_that("Bootstrap", {
  bootstrap_summary <- with_seed({
    bootstrap(rnorm(20), n = 100, do = mean)
  }, seed = 12345)
  expect_snapshot(bootstrap_summary)

  bootstrap_values <- with_seed({
    bootstrap(rnorm(20), n = 100, do = mean, f = function(x) { x })
  }, seed = 12345)
  expect_snapshot(bootstrap_values)
})
test_that("Jackknife", {
  x <- with_seed(rnorm(20), seed = 12345)
  expect_snapshot(jackknife(x, do = mean))
  expect_snapshot(jackknife(x, do = mean, f = function(x) { x }))
})
