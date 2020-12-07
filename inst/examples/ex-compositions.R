## Create a relative frequencies matrix
set.seed(123)
A <- matrix(data = sample(1:10, 50, TRUE), nrow = 10)
B <- as_abundance(A)

## Compositionnal mean
mean(B) # 0.1807357 0.2381069 0.1920732 0.2102428 0.1788414

## Metric variance (or total variance)
var(B) # 1.270878

## Metric standard deviation
sd(B) # 0.5636662

## Variation matrix
variation(B)
