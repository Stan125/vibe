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

  # Obtain family
  fam <- family(object)

  if (gof == "hier.part") {
    # Get all combinations
    combins <- acc(k = ncol(base_df) - 1)$combs

    # Name models
    model_ids <- apply(combins, MARGIN = 1, FUN = function(x)
      return(as.character(x[x > 0])))
    model_ids <- sapply(model_ids, FUN = function(x)
      return(do.call(paste0, as.list(c("x", x)))))
    model_ids <- c("x0", model_ids)

    # Fit models (empty model first) and get goodness of fit
    m0 <- glm(base_df[, 1] ~ 1, family = fam)
    gofs <- pcapply(combins, ncores = ncores, FUN = function(x) {
      m <- glm(base_df[, 1] ~ base_df[, -c(1)], family = fam)
      res <- gof(m, gofmetric = gofmetric, m0 = m0)
      return(res)
    })
    gofs <- c(gof(m0, m0 = m0))

    # Name vector with ID's
    names(gofs) <- model_ids
    gof_list <- list(gofs = list(gofs),
                     model_ids = model_ids,
                     expl_names = colnames(base_df[, -c(1)]),
                     npar = 1,
                     gof = gofmetric)
    # Do hierarchical partitioning


  }
}
