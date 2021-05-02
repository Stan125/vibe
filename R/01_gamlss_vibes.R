#' @importFrom stats model.frame
#' @importFrom stats family
#' @export

vibe.gamlss <- function(object,
                        metric = "hp",
                        gofmetric = "R2e",
                        ncores = 1,
                        progress = TRUE,
                        ...) {

  # Defensive Programming - is everything supplied the way it should be?
  error_handling(object = object,
                 metric = metric,
                 gofmetric = gofmetric,
                 progress = progress)

  # Obtain data
  base_df <- model.frame(object)
  depvar <- base_df[, 1]
  depvar_name <- names(base_df)[1]
  expl_df <- base_df[, -c(1)]

  # Obtain family
  fam <- family(object)

  # Model Class - MC with added EE since it sounds cool
  mcee <- class_finder(object)

  ## Obtain parameters connected to expl variables
  modeled_pars <- det_npar(object)

  if (metric == "hp") {

    ## Get gofs for each par
    gofs <- lapply(modeled_pars, FUN = function(par) {
      base_df_par <- model.frame(object, what = par)

      # Get expl df's
      if (depvar_name %in% colnames(base_df_par)) {
        expl_df_par <- base_df_par[, -c(which(colnames(base_df_par) == depvar_name))]
      } else {
        expl_df_par <- base_df_par
      }


      ## Obtain model ids
      model_ids_par <- mids(ncol(expl_df_par))

      ## Get gofs
      gofs <- fit_and_gof(depvar = depvar,
                          expl_df = expl_df_par,
                          fam = fam,
                          ncores = ncores,
                          progress = progress,
                          gofmetric = gofmetric,
                          class = mcee,
                          depvar_name = depvar_name,
                          base_df = base_df_par,
                          param = par)

      # Name vector with ID's
      names(gofs) <- model_ids_par

      # Return gofs
      return(gofs)

    })

    # Get expl names
    expl_names <- lapply(modeled_pars, FUN = function(par) {
      base_df_par_names <- colnames(model.frame(object, what = par))
      if (depvar_name %in% base_df_par_names)
        base_df_par_names <- base_df_par_names[-which(base_df_par_names == depvar_name)]
      return(base_df_par_names)
    })

    gof_list <- list(gofs = gofs,
                     expl_names = expl_names,
                     npar = length(modeled_pars),
                     gof = gofmetric,
                     metric = metric)

    # Do hierarchical partitioning
    gof_res <- part(gof_list)

    # Summarize into nice format
    result <- make_vibe(results = gof_res,
                        depvar_name = depvar_name,
                        metric = metric,
                        class = mcee)

    # Return
    return(result)

  } else if (metric == "relweights") {

    # If anything different than r2e don't do it
    if (gofmetric != "R2e")
      stop("Currently only metric 'R2e' implemented")

    # Do rel weights for each param
    ## Get gofs for each par
    relweight_res <- lapply(modeled_pars, FUN = function(par) {
      expl_df_par <- model.frame(object, what = par)

      # Get expl df's
      if (depvar_name %in% colnames(expl_df_par))
        expl_df_par <- expl_df_par[, -c(which(colnames(expl_df_par) == depvar_name))]

      if (par != "mu") {
        expl_df_mu <- model.frame(object, what = "mu")
        if (depvar_name %in% colnames(expl_df_mu))
          expl_df_mu <- expl_df_mu[, -c(which(colnames(expl_df_mu) == depvar_name))]
      } else {
        expl_df_mu <- NULL
      }

      # Relative Weights
      relweight_res <- rel_weights(expl_df = expl_df_par,
                                   fam = fam,
                                   depvar = depvar,
                                   gofmetric = gofmetric,
                                   class = mcee,
                                   param = par,
                                   expl_df_mu = expl_df_mu)
    })
    relweight_res <- do.call("rbind", args = relweight_res)
    row.names(relweight_res) <- NULL

    # Summarize into nice format
    result <- make_vibe(results = relweight_res,
                        depvar_name = depvar_name,
                        metric = metric,
                        class = mcee)

    # Return
    return(result)
  }
}
