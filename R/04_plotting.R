#' Plotting
#'
#' Plot function for 'vibe' objects
#' @param x A 'vibe' object created with \link{vibe}.
#' @param ... 'Additional arguments'
#' @import ggplot2
#' @export
plot.vibe <- function(x, ...) {

  if (!exists("perc"))
    perc <- FALSE

  if (perc) {
    gg <- ggplot(x$results, aes(x = var, y = indep_perc)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Variables", y = "%",
           title = paste0("Percentages of independent effects on '",
                          x$depvar_name,"'"),
           subtitle = paste0("Modeled parameters: '",
                             paste(unique(x$results$param), collapse = ", "), "'\n",
                             "Used metric: '", x$metric, "'")) +
      theme_bw() +
      coord_flip()
  } else {
    gg <- ggplot(x$results, aes(x = var, y = indep_effects)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Variables", y = "Amount of change in GOF",
           title = paste0("Percentages of independent effects on '",
                          x$depvar_name,"'"),
           subtitle = paste0("Modeled parameters: '",
                             paste(unique(x$results$param), collapse = ", "), "'\n",
                             "Used metric: '", x$metric, "'")) +
      theme_bw() +
      coord_flip()
  }

  # if multiple pars
  if (x$npar == 2)
    gg <- gg +
      facet_wrap(~ param)

  # Return plot
  return(gg)
}
