## Create fake data for testing
library(charlatan)

n <- 50
set.seed(12345)
artefact <- data.frame(
  id = as.integer(ch_integer(n)),
  doi = ch_missing(ch_doi(n)),
  color = ch_missing(ch_color_name(n = n)),
  lat = ch_lat(n),
  long = ch_lon(n),
  height = ch_missing(abs(ch_double(n))),
  width = ch_missing(abs(ch_double(n)))
)

m <- 10
## Add empty strings
k <- sum(!is.na(artefact$color))
artefact$color[sample(which(!is.na(artefact$color)), ifelse(k < m, k, m))] <- ""

## Add zeros
k <- sum(!is.na(artefact$height))
artefact$height[sample(which(!is.na(artefact$height)), ifelse(k < m, k, m))] <- 0
k <- sum(!is.na(artefact$width))
artefact$width[sample(which(!is.na(artefact$width)), ifelse(k < m, k, m))] <- 0

## Add infinite values
artefact$lat[2] <- Inf
artefact$long[2] <- Inf

## Write
write.csv(artefact, "inst/tinytest/fake.csv")
