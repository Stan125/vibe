# ---- This script tests for smaller functions ----

# ---- Libraries ----
library("vibe")
library("testthat")

# ---- is_any_multiple_classes() ----
char <- "character"
num <- 4
expect_true(is_any_multiple_classes(char, c("character", "numeric")))
expect_false(is_any_multiple_classes(num, c("character", "data.frame")))
