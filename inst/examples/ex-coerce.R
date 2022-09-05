## Create a matrix
A <- matrix(data = sample(0:10, 100, TRUE), nrow = 20, ncol = 5)

## Transform to long data.frame
head(as_long(A))
