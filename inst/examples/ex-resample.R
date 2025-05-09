## Uniform distribution
x <- rnorm(20)
resample_uniform(x, n = 10)

## Multinomial distribution
x <- sample(1:100, 20, TRUE)
resample_multinomial(x, n = 10)
