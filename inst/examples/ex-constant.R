## Create a data.frame
X <- data.frame(A = 1, B = 1:3)
X

remove_constant(X)

## Add NA
X[1, 1] <- NA
remove_constant(X)
remove_constant(X, na.rm = TRUE)
