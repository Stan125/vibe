#' Plotting
#'
#' Plot function for 'vibe' objects
#' @param x A 'vibe' object created with \link{vibe}.
#' @param ... 'Additional arguments'
#' @param perc Should percentages be shown, or raw differences in
#' goodness-of-fit?
#' @import ggplot2
#' @importFrom rlang .data
#' @export
plot.vibe <- function(x, ..., perc = FALSE) {
  if (perc) {
    gg <- ggplot(x$results, aes(x = .data$var, y = .data$indep_perc)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(
        x = "Variables", y = "%",
        title = paste0(
          "Percentages of independent effects on '",
          x$depvar_name, "'"
        ),
        subtitle = paste0(
          "Modeled parameters: '",
          paste(unique(x$results$param), collapse = ", "), "'\n",
          "Used metric: '", x$metric, "'"
        )
      ) +
      theme_bw() +
      coord_flip()
  } else {
    gg <- ggplot(x$results, aes(x = .data$var, y = .data$indep_effects)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(
        x = "Variables", y = "Amount of change in GOF",
        title = paste0(
          "Percentages of independent effects on '",
          x$depvar_name, "'"
        ),
        subtitle = paste0(
          "Modeled parameters: '",
          paste(unique(x$results$param), collapse = ", "), "'\n",
          "Used metric: '", x$metric, "'"
        )
      ) +
      theme_bw() +
      coord_flip()
  }

  # if multiple pars
  if (x$npar == 2) {
    gg <- gg +
      facet_wrap(~param)
  }

  # Return plot
  return(gg)
}
