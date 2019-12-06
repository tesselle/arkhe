## Create a count data matrix
A <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10,
                 dimnames = list(LETTERS[1:10], NULL))

## Set geographic coordinates as a matrix
B <- matrix(data = sample(0:10, 30, TRUE), nrow = 10, ncol = 3)
set_coordinates(A) <- B
get_coordinates(A)

## Set geographic coordinates as a data.frame
B <- data.frame(sample(0:10, 5, TRUE), sample(0:10, 5, TRUE), sample(0:10, 5, TRUE),
                row.names = LETTERS[seq(1, 10, 2)])
set_coordinates(A) <- B
get_coordinates(A)

## Set geographic coordinates as a list
B <- list(X = sample(0:10, 10, TRUE), Y = sample(0:10, 10, TRUE))
set_coordinates(A) <- B
get_coordinates(A)

## Unset geographic coordinates
set_coordinates(A) <- NULL
get_coordinates(A)
