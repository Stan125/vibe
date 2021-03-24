# ---- This script tests for wrong specifications of arguments ----

# ---- Libraries ----
library("vibe")
library("testthat")



# ---- Wrong specifications ----
expect_error(vibe::vibe(object = iris))
expect_error(vibe::vibe(object = iris))
