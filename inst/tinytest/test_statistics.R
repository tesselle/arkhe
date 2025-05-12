# Highest density regions ======================================================
expect_equal_to_reference(interval_hdr(faithful$eruptions),
                          file = "_snaps/interval_hdr.rds")

# Bayesian credible interval ===================================================
expect_equal_to_reference(interval_credible(faithful$eruptions),
                          file = "_snaps/interval_credible.rds")

# Confidence interval for the mean =============================================
x <- c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,
       16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5, 15.5,
       15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8, 19.7, 15, 21.4)

conf_student <- confidence_mean(x, type = "student")
expect_equal(round(conf_student, 5), c(lower = 17.91768, upper = 22.26357))
conf_normal <- confidence_mean(x, type = "normal")
expect_equal(round(conf_normal, 5), c(lower = 18.00243, upper = 22.17882))

# Confidence interval for binomial proportions =================================
conf_bin <- confidence_binomial(118, n = 236)
expect_equal(round(conf_bin, 5), c(lower = 0.43621, upper = 0.56379))

# Confidence interval for binomial proportions =================================
x <- c(35, 74, 22, 69)

expect_equal_to_reference(confidence_multinomial(x),
                          file = "_snaps/confidence_multinomial.rds")
expect_equal_to_reference(confidence_multinomial(x, corrected = TRUE),
                          file = "_snaps/confidence_multinomial_corrected.rds")

# Bootstrap confidence interval ================================================
x <- c(73.75, 99.667, 100.917, 79.583, 96.917, 96.333, 46.5, 117.5,
       123.25, 68.083, 77, 94, 72.417, 99.083, 122.333, 72.417, 94.25,
       90, 83.5, 106.917, 95.25, 119.417, 114.25, 65.5, 133.75, 139.5,
       89, 142.667, 98.167, 113.5)
y <- c(349.807, 1266.241, 1265.77, 335.965, 1332.381, 1300.963, 136.604,
       1134.438, 2339.516, 343.77, 414.958, 260.972, 313.937, 1300.992,
       1439.713, 292.131, 1344.793, 1555.306, 288.16, 1389.354, 1260.655,
       1288.423, 519.724, 146.618, 2412.891, 1267.188, 1361.083, 2228.755,
       1461.22, 1135.91)

expect_identical(
  suppressWarnings(round(confidence_bootstrap(x, level = 0.95, t0 = 108.083, type = "normal"), 3)),
  c(lower = 73.588, upper = 163.716)
)
expect_identical(
  suppressWarnings(confidence_bootstrap(x, level = 0.95, t0 = 108.083, type = "basic")),
  c(lower = 73.499, upper = 169.666)
)
expect_identical(
  suppressWarnings(round(confidence_bootstrap(x, level = 0.95, t0 = 108.083, var_t0 = 1417.715, var_t = y, type = "student"), 3)),
  c(lower = 74.852, upper = 306.475)
)
expect_identical(
  suppressWarnings(confidence_bootstrap(x, level = 0.95, type = "percentiles")),
  c(lower = 46.5, upper = 142.667)
)

# Resample =====================================================================
## Uniform distribution
x <- rnorm(20)
expect_identical(dim(resample_uniform(x, n = 10)), c(10L, 20L))

## Multinomial distribution
x <- sample(1:100, 20, TRUE)
expect_identical(dim(resample_multinomial(x, n = 10)), c(10L, 20L))

# Bootstrap ====================================================================
bootstrap_summary <- arkhe:::with_seed({
  bootstrap(rnorm(20), n = 100, do = mean)
}, seed = 12345)
expect_equal_to_reference(bootstrap_summary,
                          file = "_snaps/bootstrap_summary.rds")

bootstrap_values <- arkhe:::with_seed({
  bootstrap(rnorm(20), n = 100, do = mean, f = function(x) { x })
}, seed = 12345)
expect_equal_to_reference(bootstrap_values,
                          file = "_snaps/bootstrap_values.rds")

# Jackknife ====================================================================
x <- arkhe:::with_seed(rnorm(20), seed = 12345)
expect_equal_to_reference(jackknife(x, do = mean),
                          file = "_snaps/jackknife_summary.rds")
expect_equal_to_reference(jackknife(x, do = mean, f = function(x) { x }),
                          file = "_snaps/jackknife_values.rds")
