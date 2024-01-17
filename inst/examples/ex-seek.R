## Seek columns
seek_columns(iris, select = startsWith, prefix = "Sepal")

## Get columns
x <- get_columns(iris, select = startsWith, prefix = "Sepal")
head(x)
