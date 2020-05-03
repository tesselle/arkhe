## Create count data matrix
A <- matrix(data = sample(0:10, 100, TRUE), nrow = 20, ncol = 5)
B <- matrix(data = sample(0:10, 100, TRUE), nrow = 20, ncol = 5)

## Compare
A == B
A != B
A > 5
A < 5
A <= 10
A >= 10

## Arith
A + B
A * 2
A ^ 2

## Summary
max(A)
min(A)
range(A)
prod(A)
sum(A)
any(A)
all(A)

## Logic
A & B
A | B
