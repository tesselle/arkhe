x <- rnorm(20)

## Bootstrap
bootstrap(x, do = mean, n = 100)

## Estimate the 25th and 95th percentiles
quant <- function(x) { quantile(x, probs = c(0.25, 0.75)) }
bootstrap(x, n = 100, do = mean, f = quant)

## Get the n bootstrap estimates
(z <- bootstrap(x, n = 100, do = mean, f = function(x) { x }))

## Basic bootstrap confidence interval
confidence_bootstrap(z, level = 0.95, type = "basic", t0 = mean(x))
