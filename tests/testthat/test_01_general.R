# ---- This script tests for the correct use of ghp ----

# ---- Libraries ----
library("vibe")
library("testthat")
library("mgcv")

# Remove everything
rm(list = ls())

# Data
india <- vibe::india
sat <- subset(vibe::sat, select = -c(lab_services, time_waiting,
                                     checkup, waiting_room, hospital_room,
                                     drug_availability, aval_specialists,
                                     dr_communication))

# ---- Fitting the models ----

# GLM
glm_bin <- glm(formula = stunting ~ ., data = india)

# GAM
R <- length(unique(sat$satisfaction))
n <- names(sat)
f <- as.formula(paste("satisfaction ~", paste(n[-1], collapse = " + ")))
gam_ocat <- gam(f, data = sat, family = ocat(R = R))

# ---- Calculating variable importance - GLM ----
hp_glm <- vibe(glm_bin, metric = "hp", gofmetric = "R2e", progress = FALSE)
rw_glm <- vibe(glm_bin, metric = "relweights", gofmetric = "R2e")
print(hp_glm)
print(rw_glm)
summary(hp_glm)
summary(rw_glm)

# ---- Calculating variable importance - GAM ----
hp_gam <- vibe(gam_ocat, metric = "hp", gofmetric = "R2e", progress = FALSE)

