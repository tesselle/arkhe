test_that("Boostrap", {
  x <- c(89, 76, 92, 71, 63, 98, 50, 85, 87, 54, 95, 50, 57, 59, 53, 92, 84,
         57, 54, 91, 74, 54, 90, 97, 71, 66, 94, 62, 54, 52)
  with_seed(12345, {
    # With quantiles
    boot1 <- bootstrap(x, do = min, probs = c(0.05, 0.95), n = 30)
    expect_snapshot(boot1)
  })
  with_seed(12345, {
    # Without quantiles
    boot2 <- bootstrap(x, do = min, probs = NULL, n = 30)
    expect_snapshot(boot2)
  })
})
test_that("Jackknife", {
  x <- 1:10
  expected <- c(mean = 49.5, bias = -49.5, error = 8.616844)
  expect_equal(jackknife(x, do = sum), expected)
})
test_that("Confidence interval", {
  with_seed(12345, {
    x <- sample(1:300, 100, TRUE)
    ci <- confidence_mean(x, level = 0.95, type = "student")
    tci <- t.test(x, conf.level = 0.95)$conf.int
    expect_equal(ci, tci, ignore_attr = TRUE)
  })
})
