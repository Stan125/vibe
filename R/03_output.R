#' Coerce results from algorithms into vibes object
#'
#' @keywords internal
make_vibe <- function(results, metric, depvar_name, class) {

  # Create object for hierarchical partitioning
  if (metric == "hp") {
    vibing_list <- list(
      metric = metric,
      depvar_name = depvar_name,
      npar = results$npar,
      class = class,
      gof = results$gof,
      results = results$results[, c("var", "param",
                                    "indep_effects", "indep_perc")]
    )
  }

  # Create object for relative weights
  if (metric == "relweights") {
    vibing_list <- list(
      metric = metric,
      depvar_name = depvar_name,
      npar = 1,
      class = class,
      gof = "R2e",
      results = results
    )
  }

  class(vibing_list) <- "vibe"
  return(vibing_list)
}

#' Print function for 'vibe' objects
#'
#' @export
print.vibe <- function(vibe_object) {
  string <- paste0(
    "A 'vibe' object with the following properties:\n",
    "Metric: ", vibe_object$metric, "\n",
    "Model class: ", vibe_object$class, "\n",
    "Dep. Variable: ", vibe_object$depvar_name, "\n",
    "Goodness of fit: ", vibe_object$gof, ".\n"
  )
  cat(string)
}

#' Summary function for 'vibe' objects
#'
#' @export
summary.vibe <- function(vibe_object) {

  # Basic info
  cat("Variable Importance Results:\n")
  str <- paste0(
    "Metric: ", vibe_object$metric, "\n",
    "Model class: ", vibe_object$class, "\n",
    "Dep. Variable: ", vibe_object$depvar_name, "\n",
    "Goodness of fit: ", vibe_object$gof, "\n",
    "Number of Parameters modeled:", vibe_object$npar, "\n"
  )
  cat(str)

  # Results table
  cat("Independent contributions:\n")
  print(as.data.frame(vibe_object$results))
}
