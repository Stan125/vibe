#' Compute Relative Weights
#'
#' @keywords internal
#' @importFrom fastDummies dummy_columns
#' @importFrom stats aggregate
rel_weights <- function(expl_df, depvar, fam, class, gofmetric) {

  # What if there's categorical variables?
  non_numeric_vars <- sapply(expl_df, FUN = function(x) return(!is.numeric(x)))
  has_non_numeric <- any(non_numeric_vars)
  if (has_non_numeric) {
    expl_df_num <- expl_df
    colnames(expl_df_num) <- as.character(seq(1, ncol(expl_df_num)))
    expl_df_num <- dummy_columns(expl_df_num,
                                 remove_first_dummy = TRUE,
                                 remove_selected_columns = TRUE)
  } else {
    expl_df_num <- expl_df
  }

  # mingle with X
  X <- apply(expl_df_num, 2, scale)

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

  # Put effects from same variable together
  if (has_non_numeric) {
    same_effect_ind <- sapply(strsplit(colnames(expl_df_num), "_"),
                              FUN = function(x) return(x[1]))
    effect_summed <- aggregate(eps_new ~ same_effect_ind, FUN = sum)[, 2]
    eps_new <- effect_summed
  }

  # Percentage weights
  eps_perc <- c(eps_new / sum(eps_new))

  # Return
  ret_df <- data.frame(var = colnames(expl_df),
                       indep_effects = eps_new,
                       indep_perc = eps_perc)
  return(ret_df)
}
