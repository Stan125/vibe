#' Compute Relative Weights
#'
#' @keywords internal
rel_weights <- function(expl_df, depvar, fam, class, gofmetric) {

  # mingle with X
  X <- apply(expl_df, 2, scale)

  # Get orthogonal variables
  svdX <- svd(X)
  Z <- svdX$u %*% t(svdX$v)

  # Standardize Z
  Z <- apply(Z, 2, scale)

  # Get coefficients of X/Z relation
  Lambda <- solve(t(Z) %*% Z) %*% t(Z) %*% X

  if (class == "glm") {
    # Get unstandardized coefficients
    mfull <- glm(depvar ~ Z, family = fam)
    coefs <- coef(mfull)[2:(ncol(X) + 1)]

    # Get S
    s <- sd(predict(mfull, type = "response"))

    # Get gof (currently only R2e possible)
    m0 <- glm(depvar ~ 1, family = fam)
    gofmod <- gof(mfull, gofmetric = gofmetric, m0 = m0)
  }

  # Weights
  betaM_new <- coefs * sqrt(gofmod) / s
  eps_new <- Lambda^2 %*% betaM_new^2
  eps_perc <- c(eps_new / sum(eps_new))

  # Return
  ret_df <- data.frame(var = colnames(expl_df),
                       indep_effects = eps_new,
                       indep_perc = eps_perc)
  return(ret_df)
}
