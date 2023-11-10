#' @title Obtain goodness-of-fit figure from regression object
#' @description blabla
#'
#' @param object Regression object. One of \code{vibe:::supported_classes}.
#' @param ... Additional arguments
#' @export
obtain_gof <- function(object, ...) {
  UseMethod("obtain_gof", object)
}

#' @importFrom stats logLik
#' @export

obtain_gof.default <- function(object, gof = "R2e", m0 = NULL, ...) {
  if (!is_any_multiple_classes(object, c("glm", "gam", "gamlss"))) {
    stop("Doesn't have the correct classes")
  }
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
