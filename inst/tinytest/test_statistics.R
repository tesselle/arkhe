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
