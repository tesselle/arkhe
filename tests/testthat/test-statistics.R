test_that("Non-parametric boostrap", {
  with_seed(12345, {
    x <- rnorm(20)
    rboot <- bootstrap(x, do = mean, n = 30)
  })

  # With quantiles and confidence interval
  rboot1 <- summary(rboot, level = 0.95, probs = c(0.05, 0.95))
  expect_snapshot(rboot1)
})
test_that("Multinomial boostrap", {
  with_seed(12345, {
    x <- sample(1:100, 50, TRUE)
    mboot <- bootstrap(x, do = min, n = 30)
  })

  # With quantiles and confidence interval
  boot1 <- summary(mboot, level = 0.95, probs = c(0.05, 0.95))
  expect_snapshot(boot1)

  # Without confidence interval
  boot2 <- summary(mboot, level = NULL, probs = c(0.05, 0.95))
  expect_snapshot(boot2)

  # Without quantiles
  boot3 <- summary(mboot, level = 0.95, probs = NULL)
  expect_snapshot(boot3)
})
test_that("Jackknife", {
  with_seed(12345, {
    x <- rnorm(20)
    jack <- jackknife(x, do = mean)
  })

  jack1 <- summary(jack)
  expect_snapshot(jack1)
})
test_that("Confidence interval", {
  with_seed(12345, {
    x <- sample(1:300, 100, TRUE)
    ci <- confidence_mean(x, level = 0.95, type = "student")
    tci <- t.test(x, conf.level = 0.95)$conf.int
    expect_equal(ci, tci, ignore_attr = TRUE)
  })
})
