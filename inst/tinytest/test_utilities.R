# NULL OR operator =============================================================
expect_equal(`%||%`(NULL, 1), 1)
expect_equal(`%||%`(0, 1), 0)

# Concatenates =================================================================
expect_equal(`%+%`("foo", "bar"), "foobar")
expect_equal(`%+%`(character(), "bar"), character())
expect_equal(`%+%`("foo", character()), character())
expect_equal(`%+%`(character(), character()), character())
