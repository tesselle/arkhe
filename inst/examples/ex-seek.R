## Seek columns
seek_columns(iris, select = startsWith, prefix = "Petal")
seek_columns(iris, names = c("Petal.Length", "Petal.Width"))

## Get columns
x <- get_columns(iris, select = startsWith, prefix = "Petal")
head(x)

x <- get_columns(iris, names = c("Petal.Length", "Petal.Width"))
head(x)
