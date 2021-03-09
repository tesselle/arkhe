## Create a count data matrix
X <- CountMatrix(sample(1:10, 25, TRUE), nrow = 5, ncol = 5)
k <- sample(1:25, 3, FALSE)

## Add zeros
X[k] <- 0L
## Remove row with zeros
remove_zero(X, margin = 1)
## Remove column with zeros
remove_zero(X, margin = 2)

## Add NA
X[k] <- NA
## Remove row with zeros
remove_NA(X, margin = 1)
## Remove column with zeros
remove_NA(X, margin = 2)
