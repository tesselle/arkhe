## Read fake data
artefacts <- read.csv(system.file("tinytest/fake.csv", package = "arkhe"))

# Describe =====================================================================
expect_stdout(describe(artefacts))
