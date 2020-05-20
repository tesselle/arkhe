## Create an incidence (presence/absence) matrix
## Data will be coerced with as.logical()
A <- IncidenceMatrix(data = sample(0:1, 100, TRUE, c(1, 1/3)), nrow = 20)
## Create a count data matrix
B <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 20)

## Access
dim(B) # Get the matrix dimensions
row(B) # Get the row indexes
col(B, as.factor = TRUE) # Get the column indexes
nrow(B) # Get the number of rows
ncol(B) # Get the number of columns
dimnames(B) # Get the dimension names
rownames(B) <- LETTERS[1:20] # Set the row names
rownames(B) # Get the rownames
colnames(B) <- letters[21:25] # Set the column names
colnames(B) # Get the column names

## Subset
B[[1, 1]] # Get the first value
B[, ] # Get all values
B[1, ] # Get the first row
B[, 1] # Get the first column
B[c("A", "B", "C"), ] # Get the first three rows
B[c("D", "E", "F"), 1, drop = FALSE] # Get 3 rows of the first column
