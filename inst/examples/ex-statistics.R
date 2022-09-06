x <- seq(from = -4, to = 4, by = 0.01)
y <- dnorm(x)

confidence(y, type = "student")
confidence(y, type = "normal")
