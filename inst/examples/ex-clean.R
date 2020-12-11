## Create a count data matrix
A <- CountMatrix(sample(1:10, 25, TRUE), nrow = 5, ncol = 5)
A[sample(1:25, 3, FALSE)] <- 0 # Add zeros
A

## Remove row with zeros
remove_zero(A, margin = 1)

## Remove column with zeros
remove_zero(A, margin = 2)
