##### This script tests for the correct use of ghp #####

## Libraries
library("vibe")
library("testthat")

## Remove everything
rm(list = ls())

## Data
india$csex <- as.numeric(india$csex)
india$ctwin <- as.numeric(india$ctwin)
### --- Fitting the models --- ###

## GLM
glm_bin <- glm(formula =
                 stunting ~ cage + csex + breastfeeding + ctwin + mage + mbmi,
               data = india)

### --- Trying vibe --- ###
vibe::vibe(glm_bin, metric = "hp", gofmetric = "R2e", progress = FALSE)
vibe::vibe(glm_bin, metric = "relweight", gofmetric = "R2e")
