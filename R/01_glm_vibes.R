#' @importFrom stats model.frame
#' @importFrom stats family
#' @export

vibe.glm <- function(object,
                     metric = "hp",
                     gofmetric = "R2e",
                     ncores = 1,
                     progress = TRUE,
                     ...) {

  # Obtain data
  base_df <- model.frame(object)
  depvar <- base_df[, 1]
  expl_df <- base_df[, -c(1)]

  # Obtain family
  fam <- family(object)

  # Model Class - MC with added EE since it sounds cool
  mcee <- supported_classes[supported_classes %in% class(glm_bin)]

  if (metric == "hp") {

    ## Obtain model ids
    model_ids <- mids(ncol(expl_df))

    ## Fit models and get goodnesses of fit
    gofs <- fit_and_gof(depvar = depvar,
                        expl_df = expl_df,
                        fam = fam,
                        ncores = ncores,
                        progress = progress,
                        gofmetric = gofmetric,
                        class = mcee)

    # Name vector with ID's
    names(gofs) <- model_ids
    gof_list <- list(gofs = list(mu = gofs),
                     model_ids = model_ids,
                     expl_names = colnames(base_df[, -c(1)]),
                     npar = 1,
                     gof = gofmetric,
                     metric = metric)

    # Do hierarchical partitioning
    gof_res <- part(gof_list)

    # Summarise into nice format
    result <- make_vibe(gof_res)

    # Return
    return(result)

  } else if (metric == "relweight") {

    # If anything different than r2e don't do it
    if (gofmetric != "R2e")
      stop("Currently only metric 'R2e' implemented")

    # mingle with X
    X <- apply(expl_df, 2, scale)

    # Get orthogonal variables
    svdX <- svd(X)
    Z <- svdX$u %*% t(svdX$v)

    # Standardize Z
    Z <- apply(Z, 2, scale)

    # Get coefficients of X/Z relation
    Lambda <- solve(t(Z) %*% Z) %*% t(Z) %*% X

    # Get unstandardized coefficients
    mfull <- glm(depvar ~ Z, family = fam)
    coefs <- coef(mfull)[2:(ncol(X) + 1)]

    # Get S
    s <- sd(predict(mfull))

    # Get gof (currently only R2e possible)
    m0 <- glm(depvar ~ 1, family = fam)
    gofmod <- gof(mfull, gofmetric = gofmetric, m0 = m0)

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
}
