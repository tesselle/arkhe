## Create a count data matrix
X <- CountMatrix(data = sample(1:10, 45, TRUE), nrow = 15)
summary(X)

## Summary by groups
set_groups(X) <- rep(c("A", "B", "C"), each = 5)
summary(X)
