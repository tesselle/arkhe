## Create an incidence (presence/absence) matrix
## Data will be coerced with as.logical()
A <- IncidenceMatrix(data = sample(0:1, 100, TRUE, c(1, 1/3)), nrow = 10)

## Create a count data matrix
B <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 10)
## Coerce to presence/absence
C <- as_incidence(B)
