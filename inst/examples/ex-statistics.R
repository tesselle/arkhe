## Jackknife
x <- rnorm(20)
jack <- jackknife(x, do = mean) # Sample mean
summary(jack)

## Random samples from x with replacement
x <- rnorm(20) # numeric
boot <- bootstrap(x, do = mean, n = 100) # Sample mean
summary(boot)

## Sample observations from a multinomial distribution
x <- sample(1:100, 100, TRUE) # integer
boot <- bootstrap(x, do = median, n = 100)
summary(boot)
