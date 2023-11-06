#' Variable Importance calculation for a `gam` object
#'
#' @description
#'
#'
#' @param object A `gam` object, typically result of [`mgcv::gam`].
#' @param metric One of `c("hp", "relweights")`, which stand for hierarchical partitioning and relative weights
#' @param gofmetric Goodness-of-fit metric, the changes of which shall be analysed
#' @param ncores Number of cores used for the model fitting process, happening in [`part_core`].
#' @param progress Boolean. Do you want to see a progress bar?
#' @param ...
#'
#' @importFrom stats model.frame
#' @importFrom stats family
#' @export

vibe.gam <- function(object,
                     metric = "hp",
                     gofmetric = "R2e",
                     ncores = 1,
                     progress = TRUE) {
  # Defensive Programming - is everything supplied the way it should be?
  error_handling(
    object = object,
    metric = metric,
    gofmetric = gofmetric,
    progress = progress
  )

  # Obtain data
  base_df <- model.frame(object)
  depvar <- base_df[, 1]
  depvar_name <- names(base_df)[1]
  expl_df <- base_df[, -c(1)]

  # Obtain family
  fam <- family(object)

  # Model Class - MC with added EE since it sounds cool
  mcee <- supported_classes[supported_classes %in% class(object)]
  if (any(mcee == "gam")) {
    mcee <- "gam"
  }

  if (metric == "hp") {
    ## Obtain model ids
    model_ids <- mids(ncol(expl_df))

    ## Fit models and get goodnesses of fit
    gofs <- fit_and_gof(
      depvar = depvar,
      expl_df = expl_df,
      fam = fam,
      ncores = ncores,
      progress = progress,
      gofmetric = gofmetric,
      class = mcee,
      depvar_name = depvar_name,
      base_df = base_df
    )

    # Name vector with ID's
    names(gofs) <- model_ids
    gof_list <- list(
      gofs = list(mu = gofs),
      model_ids = model_ids,
      expl_names = list(mu = colnames(expl_df)),
      npar = 1,
      gof = gofmetric,
      metric = metric
    )

    # Do hierarchical partitioning
    gof_res <- part(gof_list)

    # Summarize into nice format
    result <- make_vibe(
      results = gof_res,
      depvar_name = depvar_name,
      metric = metric,
      class = mcee
    )

    # Return
    return(result)
  } else if (metric == "relweights") {
    # If anything different than r2e don't do it
    if (gofmetric != "R2e") {
      stop("Currently only metric 'R2e' implemented")
    }

    # Relative Weights
    relweight_res <- rel_weights(
      expl_df = expl_df,
      fam = fam,
      depvar = depvar,
      gofmetric = gofmetric,
      class = mcee
    )

    # Summarize into nice format
    result <- make_vibe(
      results = relweight_res,
      depvar_name = depvar_name,
      metric = metric,
      class = mcee
    )

    # Return
    return(result)
  }
}
