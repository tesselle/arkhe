## Create a count data matrix
X <- matrix(sample(1:10, 25, TRUE), nrow = 5, ncol = 5)

## Add NA
k <- sample(1:25, 3, FALSE)
X[k] <- NA
X

## Count missing values in rows
count(X, f = is.na, margin = 1)
## Count non-missing values in columns
count(X, f = is.na, margin = 2, negate = TRUE)

## Find row with NA
detect(X, f = is.na, margin = 1)
## Find column without any NA
detect(X, f = is.na, margin = 2, negate = TRUE, all = TRUE)

## Remove row with any NA
compact(X, f = is.na, margin = 1, all = FALSE)
## Remove column with any NA
compact(X, f = is.na, margin = 2, all = FALSE)

## Replace NA with zeros
replace_NA(X, value = 0)
