## Create a data.frame
X <- matrix(data = sample(0:10, 50, TRUE), nrow = 10, ncol = 5)
Y <- as.data.frame(X)

## Coerce to a count matrix
Z <- as_count(Y)

## Set/get groups
set_samples(Z) <- rep(c("a", "b", "c", "d", "e"), each = 2)
get_samples(Z)

## Set/get groups
set_groups(Z) <- rep(c("A", "B"), each = 5)
get_groups(Z)

## Get/get dates
dates <- list(
  tpq = sample(1301:1400, 10, replace = TRUE),
  taq = sample(1451:1500, 10, replace = TRUE)
)
set_dates(Z) <- dates
get_dates(Z)

## Collection of features
as_features(Z)
