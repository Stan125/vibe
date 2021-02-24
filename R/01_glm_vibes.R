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

  if (metric == "hp") {

    # Get all combinations
    combins <- acc(k = ncol(base_df) - 1)$combs

    # Name models
    model_ids <- apply(combins, MARGIN = 1, FUN = function(x)
      return(as.character(x[x > 0])))
    model_ids <- sapply(model_ids, FUN = function(x)
      return(do.call(paste0, as.list(c("x", x)))))
    model_ids <- c("x0", model_ids)

    # Fit models (empty model first) and get goodness of fit
    m0 <- glm(depvar ~ 1, family = fam)
    gofs <- pcapply(combins, ncores = ncores, progress = progress, FUN = function(x) {
      m <- glm(depvar ~ ., family = fam,
               data = expl_df[, x, drop = FALSE])
      res <- gof(m, gofmetric = gofmetric, m0 = m0)
      return(res)
    })
    gofs <- c(gof(m0, m0 = m0), gofs)

    # Name vector with ID's
    names(gofs) <- model_ids
    gof_list <- list(gofs = list(mu = gofs),
                     model_ids = model_ids,
                     expl_names = colnames(base_df[, -c(1)]),
                     npar = 1,
                     gof = gofmetric)

    # Do hierarchical partitioning
    gof_res <- part(gof_list)

    # Return hierpart
    return(gof_res$results[, c("var", "indep_effects", "indep_perc")])
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
