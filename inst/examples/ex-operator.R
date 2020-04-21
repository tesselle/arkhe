## Create a count data matrix
A1 <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 20, ncol = 5)

## Row and column sums
rowSums(A1)
colSums(A1)

## Row and column means
rowMeans(A1)
colMeans(A1)
