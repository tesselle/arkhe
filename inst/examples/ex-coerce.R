## Create a count matrix
A0 <- matrix(data = sample(0:10, 100, TRUE), nrow = 20, ncol = 5)

## Coerce to absolute frequencies
A1 <- as_count(A0)

## Coerce to relative frequencies
B0 <- as_composition(A1)

## Row sums are internally stored before coercing to relative frequencies
## (use get_totals() to retrieve these values)
## This allows to restore the source data
A2 <- as_count(B0)
all(A1 == A2)

## Coerce to presence/absence
C0 <- as_incidence(A1)

## Coerce to a co-occurrence matrix
D0 <- as_occurrence(A1)

## Coerce to an S3 matrix or data.frame
X <- as.matrix(A1)
all(A0 == X)

Y <- data.frame(A1)
head(Y)
