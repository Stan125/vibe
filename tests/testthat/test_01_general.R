# ---- This script tests for the correct use of ghp ----

# ---- Libraries ----
library("vibe")
library("testthat")
library("mgcv")
library("gamlss")
library("nlme")

# Remove everything
rm(list = ls())

# Data
india <- vibe::india
sat <- subset(vibe::sat, select = -c(lab_services, time_waiting,
                                     checkup, waiting_room, hospital_room,
                                     drug_availability, aval_specialists,
                                     dr_communication))
Gasoline <- subset(Gasoline, select = -c(Sample))
Gasoline$yield <- Gasoline$yield / 100

# ---- Fitting the models ----

# GLM
glm_bin <- glm(formula = stunting ~ ., data = india)

# GAM
R <- length(unique(sat$satisfaction))
n <- names(sat)
f <- as.formula(paste("satisfaction ~", paste(n[-1], collapse = " + ")))
gam_ocat <- gam(f, data = sat, family = ocat(R = R))

# GAMLSS
gamlss_beta_mu <- gamlss(yield ~ ., data = Gasoline, fam = BE(), trace = FALSE)
gamlss_beta_sig <- gamlss(yield ~ ., sigma.formula = ~ endpoint + ASTM,
                          data = Gasoline, fam = BE(), trace = FALSE)

# ---- Calculating variable importance - GLM ----
hp_glm <- vibe(glm_bin, metric = "hp", gofmetric = "R2e", progress = FALSE)
rw_glm <- vibe(glm_bin, metric = "relweights", gofmetric = "R2e")
print(hp_glm)
print(rw_glm)
summary(hp_glm)
summary(rw_glm)

# ---- Calculating variable importance - GAM ----
hp_gam <- vibe(gam_ocat, metric = "hp", gofmetric = "R2e", progress = FALSE)
rw_gam <- vibe(gam_ocat, metric = "relweights", gofmetric = "R2e")
print(hp_gam)
print(rw_gam)
summary(hp_gam)
summary(rw_gam)

# ---- Calculating variable importance - GAMLSS ----
hp_gamlss_mu <- vibe(gamlss_beta_mu, metric = "hp", gofmetric = "R2e", progress = FALSE)
hp_gamlss_sig <- vibe(gamlss_beta_sig, metric = "hp", gofmetric = "R2e", progress = FALSE)
rw_gamlss_mu <- vibe(gamlss_beta_mu, metric = "relweights", gofmetric = "R2e")
rw_gamlss_sig <- vibe(gamlss_beta_mu, metric = "relweights", gofmetric = "R2e")
lapply(list(hp_gamlss_mu, hp_gamlss_sig, rw_gamlss_mu, rw_gamlss_sig),
       FUN = print)
lapply(list(hp_gamlss_mu, hp_gamlss_sig, rw_gamlss_mu, rw_gamlss_sig),
       FUN = summary)

