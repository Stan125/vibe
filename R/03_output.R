#' Coerce results from algorithms into vibes object
#'
#' @keywords internal
make_vibe <- function(results, metric, class) {
  if (metric == "hp") {
    vibing_list <- list(
      metric = metric,
      depvar = depvar,
      npar = npar,
      results = results$results[, c("var", "param",
                                    "indep_effects", "indep_perc")]
    )
  }
  class(vibing_list) <- "vibe"
  return(vibing_list)
}
