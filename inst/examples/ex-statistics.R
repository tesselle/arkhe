## Create count data matrix
A <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 20, ncol = 5)

## Row and column means
rowMeans(A)
colMeans(A)

## Row and column sums
rowSums(A)
colSums(A)

## Variance, covariance and correlation matrix
var(A)
cov(A)
cor(A)
