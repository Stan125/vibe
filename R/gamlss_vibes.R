#' @title Variable Importance calculation for a `gamlss` object
#'
#' @description `vibe.gamlss` takes a fitted [gamlss::gamlss()] object and calculates
#' variable importance metrics by fitting the submodels required, extracting the
#' desired goodness-of-fit metric and applying variable importance metrics to
#' it.
#' @inheritParams vibe.gam
#'
#' @importFrom stats model.frame
#' @importFrom stats family
#' @export

vibe.gamlss <- function(object,
                        varimp = "hp",
                        gof = "R2e",
                        ncores = 1,
                        progress = TRUE,
                        ...) {
  # Defensive Programming - is everything supplied the way it should be?
  error_handling(
    object = object,
    varimp = varimp,
    gof = gof,
    progress = progress
  )

  # Obtain data
  base_df <- model.frame(object)
  depvar <- base_df[, 1]
  depvar_name <- names(base_df)[1]

  # Obtain family
  fam <- family(object)
  if (length(fam) > 1) {
    fam <- fam[1]
  }

  # Model Class - MC with added EE since it sounds cool
  mcee <- class_finder(object)

  ## Obtain parameters connected to expl variables
  modeled_pars <- det_npar(object)

  if (varimp == "hp") {
    ## Get gofs for each par
    gofs <- lapply(modeled_pars, FUN = function(par) {
      base_df_par <- model.frame(object, what = par)

      # Get expl df's
      if (depvar_name %in% colnames(base_df_par)) {
        expl_df_par <-
          base_df_par[, -c(which(colnames(base_df_par) == depvar_name))]
      } else {
        expl_df_par <- base_df_par
      }


      ## Obtain model ids
      model_ids_par <- mids(ncol(expl_df_par))

      ## Get gofs
      gofs <- fit_and_gof(
        depvar = depvar,
        expl_df = expl_df_par,
        fam = fam,
        ncores = ncores,
        progress = progress,
        gof = gof,
        class = mcee,
        depvar_name = depvar_name,
        base_df = base_df_par,
        param = par
      )

      # Name vector with ID's
      names(gofs) <- model_ids_par

      # Return gofs
      return(gofs)
    })

    # Get expl names
    expl_names <- lapply(modeled_pars, FUN = function(par) {
      base_df_par_names <- colnames(model.frame(object, what = par))
      if (depvar_name %in% base_df_par_names) {
        base_df_par_names <-
          base_df_par_names[-which(base_df_par_names == depvar_name)]
      }
      return(base_df_par_names)
    })

    gof_list <- list(
      gofs = gofs,
      expl_names = expl_names,
      npar = length(modeled_pars),
      gof = gof,
      varimp = varimp
    )

    # Do hierarchical partitioning
    gof_res <- part(gof_list)

    # Summarize into nice format
    result <- make_vibe(
      results = gof_res,
      depvar_name = depvar_name,
      varimp = varimp,
      class = mcee
    )

    # Return
    return(result)
  } else if (varimp == "relweights") {
    # If anything different than r2e don't do it
    if (gof != "R2e") {
      stop("Currently only varimp 'R2e' implemented")
    }

    # Do rel weights for each param
    ## Get gofs for each par
    relweight_res <- lapply(modeled_pars, FUN = function(par) {
      expl_df_par <- model.frame(object, what = par)

      # Get expl df's
      if (depvar_name %in% colnames(expl_df_par)) {
        expl_df_par <-
          expl_df_par[, -c(which(colnames(expl_df_par) == depvar_name))]
      }

      # Relative Weights
      relweight_res <- rel_weights(
        expl_df = expl_df_par,
        fam = fam,
        depvar = depvar,
        gof = gof,
        class = mcee,
        param = par
      )

      return(relweight_res)
    })
    relweight_res <- do.call("rbind", args = relweight_res)
    row.names(relweight_res) <- NULL

    # Summarize into nice format
    result <- make_vibe(
      results = relweight_res,
      depvar_name = depvar_name,
      varimp = varimp,
      class = mcee
    )

    # Return
    return(result)
  }
}
