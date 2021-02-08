harris <- matrix(
  data = c(2, 1,
           3, 1,
           4, 1,
           5, 2,
           5, 3,
           5, 4,
           6, 5,
           7, 1,
           7, 6,
           8, 1,
           8, 6,
           9, 7,
           9, 8),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("lower", "upper"))
)

test_that("Initialize a StratigraphicMatrix instance", {
  # Empty instence
  expect_s4_class(.StratigraphicMatrix(), "StratigraphicMatrix")

  strati <- as_stratigraphy(harris)
  layers <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
              FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
              FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
              TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
              FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
              FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
              FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
              FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
              FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
              FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
              TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
              FALSE, FALSE, FALSE)
  expect_equal(strati@.Data, layers, ignore_attr = TRUE)
})
test_that("non DAG fails", {
  harris1 <- rbind(harris, c(1, 9))
  cnd <- catch_conditions(as_stratigraphy(harris1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain cycles", cnd[[1]]$message))

  harris2 <- rbind(harris, c(6, 8))
  cnd <- catch_conditions(as_stratigraphy(harris2))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain cycles", cnd[[1]]$message))
})
