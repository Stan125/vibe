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
sat <- subset(vibe::sat, select = -c(
  lab_services, time_waiting,
  checkup, waiting_room, hospital_room,
  drug_availability, aval_specialists,
  dr_communication
))
gasoline <- subset(Gasoline, select = -c(Sample))
gasoline$yield <- gasoline$yield / 100

# ---- Fitting the models ----

# GLM
glm_bin <- glm(formula = stunting ~ ., data = india)

# GAM
gam_ocat <- gam(
  satisfaction ~ admin + hygiene + time_appointment +
    quality_dr + diagnosis_exactness + equipment_modern +
    friendly_workers + parking_playingrooms_cafes,
  data = sat, family = ocat(R = 3)
)

# GAMLSS
gamlss_beta_mu <- gamlss(yield ~ ., data = gasoline, fam = BE(), trace = FALSE)
gamlss_beta_sig <- gamlss(yield ~ endpoint + ASTM,
  sigma.formula = ~endpoint,
  data = gasoline, fam = BE(), trace = FALSE
)

# ---- Calculating variable importance - GLM ----
hp_glm <- vibe(glm_bin, varimp = "hp", gof = "R2e", progress = FALSE)
rw_glm <- vibe(glm_bin, varimp = "relweights", gof = "R2e")
print(hp_glm)
print(rw_glm)
summary(hp_glm)
summary(rw_glm)

# ---- Calculating variable importance - GAM ----
hp_gam <- vibe(gam_ocat, varimp = "hp", gof = "R2e", progress = FALSE)
rw_gam <- vibe(gam_ocat, varimp = "relweights", gof = "R2e")
print(hp_gam)
print(rw_gam)
summary(hp_gam)
summary(rw_gam)

# ---- Calculating variable importance - GAMLSS ----
hp_gamlss_mu <- vibe(gamlss_beta_mu,
  varimp = "hp",
  gof = "R2e", progress = FALSE
)
hp_gamlss_sig <- vibe(gamlss_beta_sig,
  varimp = "hp",
  gof = "R2e", progress = FALSE
)
rw_gamlss_mu <- vibe(gamlss_beta_mu, varimp = "relweights", gof = "R2e")
rw_gamlss_sig <- vibe(gamlss_beta_sig, varimp = "relweights", gof = "R2e")
lapply(list(hp_gamlss_mu, hp_gamlss_sig, rw_gamlss_mu, rw_gamlss_sig),
  FUN = print
)
lapply(list(hp_gamlss_mu, hp_gamlss_sig, rw_gamlss_mu, rw_gamlss_sig),
  FUN = summary
)

# ---- Plotting ----
cur_env <- environment()
obj <- ls(envir = cur_env)
plot_list <- lapply(obj[grep("rw|hp", obj)], FUN = function(x) {
  plot(get(x, envir = cur_env))
})
pdf("vibe_plots.pdf",
  onefile = TRUE,
  width = 12, height = 7
)
for (i in seq_along(plot_list)) {
  print(plot_list[[i]])
}
dev.off()

# ---- Saving the models for re-use
save(list = obj[grep("rw|hp", obj)], file = "models.Rdata")
