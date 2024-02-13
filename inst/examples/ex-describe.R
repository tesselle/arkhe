## Create a data matrix
X <- matrix(sample(1:10, 15, TRUE), nrow = 3, ncol = 5)

## Add NA
k <- sample(1:15, 3, FALSE)
X[k] <- NA

describe(X)
