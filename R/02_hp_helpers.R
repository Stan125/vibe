#' Unique combinations of variables
#'
#' Function to get all possible combinations for a set of covariates.
#'
#' This function takes a number (\code{k}) and then outputs a matrix with every
#' row depicting one unique combination of covariates. This function is used for
#' model fitting.
#'
#' @param k Number of covariates (or covariate groups).
#'
#' @importFrom gtools combinations
#' @export

acc <- function(k) {
  ## Combinations
  combs <- as.list(1:k)
  combs <- lapply(combs, FUN = function(x)
    return(matrix(0, nrow = n_combs(k, x), ncol = k)))
  for (i in 1:length(combs)) {
    temp_comb <- combinations(k, i)
    dtc <- dim(temp_comb)
    combs[[i]][1:dtc[1], 1:dtc[2]] <- temp_comb
  }
  combs <- do.call(rbind, combs)

  ## Binary combinations
  indices <- cbind(1:nrow(combs), as.vector(combs))
  indices <- indices[indices[, 2] > 0, ]
  binary <- matrix(0, nrow = nrow(combs), ncol = ncol(combs))
  binary[indices] <- 1

  return(list(combs = combs, binary = binary))
}

#' Internal function: Calculate number of combinations
#'
#'@keywords internal

n_combs <- function(n, r)
  return(factorial(n) / (factorial(n - r) * factorial(r)))

#' Obtain model ids given a number of covariates
#'
#' @keywords internal
mids <- function(ncov) {

  # Get all combinations
  combins <- acc(k = ncov)$combs

  # Name models
  model_ids <- apply(combins, MARGIN = 1, FUN = function(x)
    return(as.character(x[x > 0])))
  model_ids <- sapply(model_ids, FUN = function(x)
    return(do.call(paste0, as.list(c("x", x)))))
  model_ids <- c("x0", model_ids)

  # Return
  return(model_ids)
}

#' Fit all possible submodels and obtain goodnesses of fit
#'
#' @keywords internal
#' @importFrom stats glm as.formula
#' @importFrom mgcv gam
fit_and_gof <- function(depvar,
                        expl_df,
                        fam,
                        ncores,
                        progress,
                        gofmetric,
                        class,
                        depvar_name,
                        base_df,
                        param,
                        object)
{

  # Get all model combinations
  combins <- acc(k = ncol(expl_df))$combs

  # GLM ----
  if (class == "glm") {
    # Fit models (empty model first) and get goodness of fit
    m0 <- glm(depvar ~ 1, family = fam)
    gofs <- pcapply(combins, ncores = ncores, progress = progress, FUN = function(x) {
      m <- glm(depvar ~ ., family = fam,
               data = expl_df[, x, drop = FALSE])
      res <- gof(m, gofmetric = gofmetric, m0 = m0)
      return(res)
    })
    gofs <- c(gof(m0, m0 = m0), gofs)
    return(gofs)
  }

  # GAM ----
  if (class == "gam") {
    varnames <- names(expl_df)
    # Fit models (empty model first) and get goodness of fit
    m0 <- gam(depvar ~ 1, family = fam)
    gofs <- pcapply(combins, ncores = ncores, progress = progress, FUN = function(x) {
      f <- as.formula(paste(depvar_name, "~", paste(varnames[x], collapse = " + ")))
      m <- gam(f, family = fam, data = base_df)
      res <- gof(m, gofmetric = gofmetric, m0 = m0)
      return(res)
    })
    gofs <- c(gof(m0, m0 = m0), gofs)
    return(gofs)
  }

  # GAMLSS ----
  if (class == "gamlss") {
    if (!(param %in% c("mu", "sigma")))
      stop("Vibe currently only supports mu and sigma parameters")

    m0 <- gamlss(depvar ~ 1, family = fam, trace = FALSE)
    varnames <- names(expl_df)
      if (param == "mu") {

        gofs <- pcapply(combins, ncores = ncores, progress = progress, FUN = function(x) {
          f <- as.formula(paste(depvar_name, "~", paste(varnames[x], collapse = " + ")))
          m <- gamlss(f, family = fam, data = base_df, trace = FALSE)
          res <- gof(m, gofmetric = gofmetric, m0 = m0)
          return(res)
        })
        gofs <- c(gof(m0, m0 = m0), gofs)

      } else if (param == "sigma") {

        gofs <- pcapply(combins, ncores = ncores, progress = progress, FUN = function(x) {
          f <- as.formula(paste("~", paste(varnames[x], collapse = " + ")))
          m <- gamlss(formula = depvar ~ 1, sigma.formula = f, family = fam[1], data = base_df,
                      trace = FALSE)
          res <- gof(m, gofmetric = gofmetric, m0 = m0)
          return(res)
        })
        gofs <- c(gof(m0, m0 = m0), gofs)

      }
      return(gofs)
    }

}

