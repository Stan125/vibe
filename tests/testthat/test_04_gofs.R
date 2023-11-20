# ---- Goodness-of-fit test script

# ---- Libraries
library("mgcv")
library("gamlss")

# ---- Load models
load("models.RData")

# ---- Fit Null models
glm_bin_m0 <- fit_null_model(glm_bin)
gam_ocat_m0 <- fit_null_model(gam_ocat)
gamlss_beta_mu_m0 <- fit_null_model(gamlss_beta_mu)
gamlss_beta_sig_m0 <- fit_null_model(gamlss_beta_sig)

# ---- Get Goodnesses-of-fit
res_df <- expand.grid(
  list(
    model = c("glm_bin", "gam_ocat", "gamlss_beta_mu", "gamlss_beta_sig"),
    gof = c("R2e", "R2Mac")
  )
)
res_df$result <- apply(res_df, MARGIN = 1, FUN = function(x) {
  obtain_gof(get(x[1]), x[2], get(paste0(x[1], "_m0")))
})
