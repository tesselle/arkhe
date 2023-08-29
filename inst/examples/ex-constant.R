## Create a data.frame
df <- data.frame(A = 1, B = 1:3)

remove_constant(df)

## Add NA
df[1, 1] <- NA
remove_constant(df)
remove_constant(df, na.rm = TRUE)
