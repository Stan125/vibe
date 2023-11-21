#' Variable Importance calculation for an `lm` object
#'
#' @description `vibe.lm` takes a fitted [lm()] object and calculates
#' variable importance metrics by fitting the submodels required, extracting the
#' desired goodness-of-fit metric and applying variable importance metrics to
#' it.
#' @param object A `lm` object, typically result of [lm()].
#' @inheritParams vibe.gam
#'
#' @importFrom stats model.frame
#' @importFrom stats family
#' @export
#' @examples
#' iris_lm <- lm(Sepal.Width ~ ., data = iris)
#' vibe(iris_lm, gof = "R2Mac", varimp = "hp")
#'
vibe.lm <- function(object,
                    varimp = "hp",
                    gof = "R2e",
                    ncores = 1,
                    progress = TRUE,
                    ...) {
  # Defensive Programming - is everything supplied the way it should be?
  args_supported(
    object = object,
    varimp = varimp,
    gof = gof,
    progress = progress
  )

  # Obtain data
  base_df <- model.frame(object)
  depvar <- base_df[, 1]
  depvar_name <- names(base_df)[1]
  expl_df <- base_df[, -c(1)]

  # Model Class - MC with added EE since it sounds cool
  mcee <- class_finder(object)

  if (varimp == "hp") {
    ## Obtain model ids
    model_ids <- mids(ncol(expl_df))

    ## Fit models and get goodness-es of fit
    gofs <- fit_and_gof(
      depvar = depvar,
      expl_df = expl_df,
      fam = NULL,
      ncores = ncores,
      progress = progress,
      gof = gof,
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
    # Relative Weights
    relweight_res <- rel_weights(
      expl_df = expl_df,
      fam = NULL,
      depvar = depvar,
      gof = gof,
      class = mcee
    )

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
