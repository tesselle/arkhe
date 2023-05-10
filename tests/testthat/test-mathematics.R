test_that("Greatest Common Divisor", {
  expect_equal(math_gcd(c(54, 48), c(24, 18)), c(6, 6))
})
test_that("Least Common Multiple", {
  expect_equal(math_lcm(c(12, 4), c(18, 3)), c(36, 12))
})
