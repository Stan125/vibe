#' @title Obtain goodness-of-fit figure from regression model object
#' @description This extracts supported goodness-of-fit metrics from supported
#'   model classes.
#' @param object Regression object. One of the supported classes documented in
#'   [scam].
#' @param ... Additional arguments
#' @export
obtain_gof <- function(object, ...) {
  UseMethod("obtain_gof", object)
}

#' @importFrom stats logLik
#' @export
obtain_gof.default <- function(object, gof = "R2e", m0 = NULL, ...) {
  args_supported(object = object, gof = gof)

  if (gof == "R2e" && is.null(m0)) {
    stop("Empty model needs to be provided")
  }

  if (gof == "R2e") {
    l0 <- as.numeric(logLik(m0))
    lm <- as.numeric(logLik(object))
    n <- length(object$y)
    return(1 - (lm / l0)^(-(2 / n) * l0))
  }
}

obtain_gof_r2e <- function(object, m0 = NULL) {

}
