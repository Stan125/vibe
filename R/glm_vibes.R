#' @importFrom stats model.frame
#' @importFrom stats family

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
    gofs <- pcapply(combins, ncores = ncores, FUN = function(x) {
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

    return(gof_res)
  } else if (metric == "relweight") {

    # mingle with X
    X <- apply(base_df, 2, scale)

    # Get orthogonal variables
    svdX <- svd(X)
    Z <- svdX$u %*% t(svdX$v)

    # Standardize Z
    Z <- apply(Z, 2, scale)

    # Get coefficients of X/Z relation
    Lambda <- solve(t(Z) %*% Z) %*% t(Z) %*% X

    # Get unstandardized coefficients
    glmY <- glm(depvar ~ Z, family = fam)
    coefs <- coef(glmY)[2:(ncol(X) + 1)]

    # Get S
    s <- sd(predict(glmY))

    # Get R2o
    loglik_empty <- c(logLik(gamlss(Y ~ 1)))
    loglik_full <- c(logLik(glmY))
    R2e <- 1 - (loglik_full / loglik_empty)^((- 2 / glmY$N) * loglik_empty)

    # Weights
    betaM_new <- coefs * sqrt(R2e) / s
    eps_new <- Lambda^2 %*% betaM_new^2
    eps_perc <- c(eps_new / sum(eps_new))

    ### Try the same with poisson regression ###
    hp_res_lm <- ghp(depname = "mage",
                     data = india,
                     gof = "r.squared",
                     method = "lm")
    hp_res_pois <- ghp(depname = "mage",
                       data = india,
                       gof = "R2e",
                       method = "gamlss",
                       family = "PO")

    ## Now try to recreate using relative weights ##
    Y <- as.matrix(india$mage)
    X <- as.matrix(india[, !colnames(india) %in% "mage"])

    # mingle with X
    X <- apply(X, 2, scale)

    # Get orthogonal variables
    svdX <- svd(X)
    Z <- svdX$u %*% t(svdX$v)

    # Standardize Z
    Z <- apply(Z, 2, scale)

    # Get coefficients of X/Z relation
    Lambda <- solve(t(Z) %*% Z) %*% t(Z) %*% X

    # Get unstandardized coefficients
    glmY <- gamlss(Y ~ Z, family = "PO")
    coefs <- coef(glmY)[2:(ncol(X) + 1)]

    # Get S
    s <- sd(predict(glmY))

    # Get R2o
    loglik_empty <- c(logLik(gamlss(Y ~ 1, family = "PO")))
    loglik_full <- c(logLik(glmY))
    R2e <- 1 - (loglik_full / loglik_empty)^((- 2 / glmY$N) * loglik_empty)

    # Weights
    betaM_new <- coefs * sqrt(R2e) / s
    eps_new <- Lambda^2 %*% betaM_new^2
    eps_perc <- c(eps_new / sum(eps_new))
  }
}
