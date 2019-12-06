## Create a count matrix
A0 <- matrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10)

## Coerce to absolute frequencies
A1 <- as_count(A0)

## Coerce to relative frequencies
B <- as_abundance(A1)

## Row sums are internally stored before coercing to a frequency matrix
## (use totals() to get these values)
## This allows to restore the source data
A2 <- as_count(B)
all(A1 == A2)

## Coerce to presence/absence
C <- as_incidence(A1)

## Coerce to a co-occurrence matrix
D <- as_occurrence(A1)

## Collection of features
set_dates(A1) <- matrix(sample(0:10, 20, TRUE), nrow = 10, ncol = 2)
set_coordinates(A1) <- matrix(sample(0:10, 30, TRUE), nrow = 10, ncol = 3)
as_features(A1)
