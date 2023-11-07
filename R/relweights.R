#' Compute Relative Weights
#'
#' @keywords internal
#' @importFrom fastDummies dummy_columns
#' @importFrom stats aggregate glm coef sd predict
#' @importFrom mgcv gam
#' @importFrom gamlss gamlss
rel_weights <- function(expl_df, depvar, fam, class, gofmetric, param = "mu") {
  # What if there's categorical variables?
  non_numeric_vars <- sapply(expl_df, FUN = function(x) {
    return(!is.numeric(x))
  })
  has_non_numeric <- any(non_numeric_vars)
  if (has_non_numeric) {
    expl_df_num <- expl_df
    colnames(expl_df_num) <- as.character(seq(1, ncol(expl_df_num)))
    expl_df_num <- dummy_columns(expl_df_num,
      remove_first_dummy = TRUE,
      remove_selected_columns = TRUE
    )
  } else {
    expl_df_num <- expl_df
  }

  # mingle with x_mat
  x_mat <- apply(expl_df_num, 2, scale)

  # Get orthogonal variables
  svd_x_mat <- svd(x_mat)
  z_mat <- svd_x_mat$u %*% t(svd_x_mat$v)

  # Standardize z_mat
  z_mat <- apply(z_mat, 2, scale)

  # Get coefficients of x_mat/z_mat relation
  lambda <- solve(t(z_mat) %*% z_mat) %*% t(z_mat) %*% x_mat

  if (class == "glm") {
    # Get unstandardized coefficients
    mfull <- glm(depvar ~ z_mat, family = fam)
    coefs <- coef(mfull)[2:(ncol(x_mat) + 1)]

    # Get S
    s <- sd(predict(mfull, type = "response"))

    # Get gof (currently only R2e possible)
    m0 <- glm(depvar ~ 1, family = fam)
    gofmod <- gof(mfull, gofmetric = gofmetric, m0 = m0)
  }

  if (class == "gam") {
    # Get unstandardized coefficients
    mfull <- gam(depvar ~ z_mat, family = fam)
    coefs <- coef(mfull)[2:(ncol(x_mat) + 1)]

    # Get S
    s <- sd(predict(mfull))

    # Get gof (currently only R2e possible)
    m0 <- gam(depvar ~ 1, family = fam)
    gofmod <- gof(mfull, gofmetric = gofmetric, m0 = m0)
  }

  if (class == "gamlss") {
    if (param == "mu") {
      # Get unstandardized coefficients
      mfull <- gamlss(depvar ~ z_mat, family = fam, trace = FALSE)
      coefs <- coef(mfull)[2:(ncol(x_mat) + 1)]

      # Get S
      s <- sd(predict(mfull))

      # Get gof (currently only R2e possible)
      m0 <- gamlss(depvar ~ 1, family = fam, trace = FALSE)
      gofmod <- gof(mfull, gofmetric = gofmetric, m0 = m0)
    } else if (param == "sigma") {
      # Get unstandardized coefficients
      mfull <- gamlss(
        depvar ~ 1,
        sigma.formula = ~z_mat,
        family = fam, trace = FALSE
      )
      coefs <- coef(mfull, what = "sigma")[2:(ncol(x_mat) + 1)]

      # Get S
      s <- sd(predict(mfull, what = "sigma"))

      # Get gof (currently only R2e possible)
      m0 <- gamlss(depvar ~ 1, family = fam, trace = FALSE)
      gofmod <- gof(mfull, gofmetric = gofmetric, m0 = m0)
    }
  }

  # Weights
  beta_m_new <- coefs * sqrt(gofmod) / s
  eps_new <- lambda^2 %*% beta_m_new^2

  # Put effects from same variable together
  if (has_non_numeric) {
    same_effect_ind <- sapply(strsplit(colnames(expl_df_num), "_"),
      FUN = function(x) {
        return(x[1])
      }
    )
    effect_summed <- aggregate(eps_new,
      list(same_effect_ind = same_effect_ind),
      FUN = sum
    )[, 2]
    eps_new <- effect_summed
  }

  # Percentage weights
  eps_perc <- c(eps_new / sum(eps_new))

  # Return
  ret_df <- data.frame(
    var = colnames(expl_df),
    param = param,
    indep_effects = eps_new,
    indep_perc = eps_perc
  )
  return(ret_df)
}
