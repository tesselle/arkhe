## Read fake data
artefacts <- read.csv(system.file("tinytest/fake.csv", package = "arkhe"))

# Sparsity =====================================================================
expect_equal(sparsity(artefacts, count = TRUE), 20)
expect_equal(sparsity(artefacts, count = FALSE), 20/250)

# Describe =====================================================================
expect_stdout(describe(artefacts))
