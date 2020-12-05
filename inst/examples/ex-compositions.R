## Create an incidence (presence/absence) matrix
A <- matrix(data = sample(1:10, 100, TRUE), nrow = 20)
B <- as_abundance(A)

## Mean
mean(B)

## Variation matrix
var(B)

## Correlation matrix
cor(B)

## CLR covariance matrix
cov(B)
