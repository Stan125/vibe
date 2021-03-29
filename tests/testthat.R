Sys.setenv("R_TESTS" = "")
library("testthat")
library("vibe")

test_check("vibe")
