## Create a count data matrix
A <- CountMatrix(data = sample(0:10, 100, TRUE), nrow = 10, ncol = 10,
                 dimnames = list(LETTERS[1:10], NULL))

## Set dates as a matrix
B <- matrix(data = sample(0:10, 20, TRUE), nrow = 10, ncol = 2)
set_dates(A) <- B
get_dates(A)

## Set dates as a data.frame
B <- data.frame(sample(0:10, 5, TRUE), sample(0:10, 5, TRUE),
                row.names = LETTERS[seq(1, 10, 2)])
set_dates(A) <- B
get_dates(A)

## Set dates as a list
B <- list(value = sample(0:10, 10, TRUE), error = sample(0:10, 10, TRUE))
set_dates(A) <- B
get_dates(A)

## Set dates as a numeric vector
B <- sample(0:10, 10, TRUE)
suppressWarnings(set_dates(A) <- B)
get_dates(A)

## Set dates as a character vector
B <- c("X", "IX", "VIII", "VII", "VI", "V", "IV", "III", "II", "I")
set_dates(A) <- B
get_dates(A)

## Unset dates
set_dates(A) <- NULL
get_dates(A)
