#' Coerce results from algorithms into vibes object
#'
#' @keywords internal
make_vibe <- function(results, varimp, depvar_name, class) {
  # Create object for hierarchical partitioning
  if (varimp == "hp") {
    vibing_list <- list(
      varimp = varimp,
      depvar_name = depvar_name,
      npar = results$npar,
      class = class,
      gof = results$gof,
      results = results$results[, c(
        "var", "param",
        "indep_effects", "indep_perc"
      )]
    )
  }

  # Create object for relative weights
  if (varimp == "relweights") {
    vibing_list <- list(
      varimp = varimp,
      depvar_name = depvar_name,
      npar = length(unique(results$param)),
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
#' @param x Any object with class 'vibe'
#' @param ... Additional arguments
#' @export
print.vibe <- function(x, ...) {
  string <- paste0(
    "A 'vibe' object with the following properties:\n",
    "Var. Imp. Metric: ", x$varimp, "\n",
    "Model class: ", x$class, "\n",
    "Dep. Variable: ", x$depvar_name, "\n",
    "Goodness of fit: ", x$gof, ".\n"
  )
  cat(string)
}

#' Summary function for 'vibe' objects
#'
#' @param object Any object with class 'vibe'
#' @param ... Additional arguments
#' @export
summary.vibe <- function(object, ...) {
  # Basic info
  cat("Variable Importance Results:\n")
  str <- paste0(
    "Var. Imp. Metric: ", object$varimp, "\n",
    "Model class: ", object$class, "\n",
    "Dep. Variable: ", object$depvar_name, "\n",
    "Goodness of fit: ", object$gof, "\n",
    "Number of Parameters modeled:", object$npar, "\n"
  )
  cat(str)

  # Results table
  cat("Independent contributions:\n")
  print(as.data.frame(object$results))
}
