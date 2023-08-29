# Error ========================================================================
X <- arkhe:::catch_conditions(throw_error("custom_error", "E"))
expect_inherits(X[[1]], c("custom_error", "error", "condition"))

X <- arkhe:::catch_message(stop("E"))
expect_equal(X, "E")

# Warning ======================================================================
X <- arkhe:::catch_conditions(throw_warning("custom_warning", "W"))
expect_inherits(X[[1]], c("custom_warning", "warning", "condition"))

X <- arkhe:::catch_message(warning("W"))
expect_equal(X, "W")

# Message ======================================================================
X <- arkhe:::catch_conditions(message("M"))
expect_inherits(X[[1]], c("simpleMessage", "message", "condition"))

X <- arkhe:::catch_message(message("M"))
expect_equal(X, "M\n")
