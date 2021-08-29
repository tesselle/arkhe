test_that("Boostrap", {
  with_seed(12345, {
    x <- c(89, 76, 92, 71, 63, 98, 50, 85, 87, 54, 95, 50, 57, 59, 53, 92, 84,
           57, 54, 91, 74, 54, 90, 97, 71, 66, 94, 62, 54, 52)
    expected <- c(min = 35.00, mean = 42.57, max = 48.00, Q05 = 35.45, Q95 = 47.55)
    boot <- bootstrap(x, do = min, n = 30)
    expect_equal(round(boot, 2), expected)
  })
})
test_that("Jackknife", {
  x <- 1:10
  expected <- c(mean = 49.5, bias = -49.5, error = 8.616844)
  expect_equal(jackknife(x, do = sum), expected)
})
