# ---- This script tests for the correct use of ghp ----

# ---- Libraries ----
library("vibe")
library("testthat")
library("mgcv")

# Remove everything
rm(list = ls())

# Data
india <- vibe::india

# ---- Fitting the models ----

# GLM
glm_bin <- glm(formula = stunting ~ ., data = india)

# GAM
gam_ocat <-

# ---- Calculating variable importance - GLM ----
hp <- vibe(glm_bin, metric = "hp", gofmetric = "R2e", progress = FALSE)
rw <- vibe(glm_bin, metric = "relweights", gofmetric = "R2e")

# ---- Calculating variable importance - GAM ----


