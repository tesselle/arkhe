X <- data.frame(
  x = 1:5,
  y = 6:10,
  row.names = LETTERS[1:5]
)

Y <- c(D = 44, B = 55, Z = 22)

append_column(X, Y, after = 3)
