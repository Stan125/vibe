#' @title Obtain goodness-of-fit figure from regression model object
#' @description This extracts supported goodness-of-fit metrics from supported
#'   model classes.
#' @param object Regression object. One of the supported classes documented in
#'   [scam].
#' @inheritDotParams obtain_gof.default
#' @param ... Additional arguments
#' @export
obtain_gof <- function(object, ...) {
  UseMethod("obtain_gof", object)
}

#' @param m0 This is the "empty model", the model without any explanatory variables.
#' @inheritParams vibe.gam
#' @export
#' @rdname obtain_gof
obtain_gof.default <- function(object, gof = "R2e", m0 = NULL, ...) {
  args_supported(object = object, gof = gof)

  if (gof == "R2e" && is.null(m0)) {
    stop("Empty model needs to be provided")
  }

  if (gof == "R2e") {
    return(obtain_gof_r2e(object, m0, depvar = object$y))
  }

  if (gof == "R2Mac") {
    return(obtain_gof_r2_mcfadden(object, m0))
  }
}

#' @param m0 This is the "empty model", the model without any explanatory variables.
#' @inheritParams vibe.gam
#' @export
#' @rdname obtain_gof
obtain_gof.lm <- function(object, gof = "R2e", m0 = NULL, ...) {
  args_supported(object = object, gof = gof)

  if (gof == "R2e" && is.null(m0)) {
    stop("Empty model needs to be provided")
  }

  if (gof == "R2e") {
    depvar <- object$model[, 1]
    return(obtain_gof_r2e(object, m0, depvar = depvar))
  }

  if (gof == "R2Mac") {
    return(obtain_gof_r2_mcfadden(object, m0))
  }
}


#' @title Obtain goodness of fit figure for Estrella's Pseudo R2
#'
#' @inheritParams obtain_gof
#'
#' @keywords internal
#' @importFrom stats logLik
#' @references Arturo Estrella (1998) A New Measure of Fit for Equations With Dichotomous Dependent Variables, Journal of Business & Economic Statistics, 16:2, 198-205, DOI: 10.1080/07350015.1998.10524753
obtain_gof_r2e <- function(object, m0, depvar) {
  l0 <- as.numeric(logLik(m0))
  lm <- as.numeric(logLik(object))
  n <- length(depvar)
  return(1 - (lm / l0)^(-(2 / n) * l0))
}

#' @title Obtain goodness of fit figure for McFadden's Pseudo R2
#'
#' @inheritParams obtain_gof
#' @importFrom stats logLik
#' @keywords internal
#' @references D. McFadden. Conditional logit analysis of qualitative choice behavior. In P. Zarembka, editor, Frontiers in Econometrics, chapter Four, pages 104–142. Academic Press, New York, 1974.
obtain_gof_r2_mcfadden <- function(object, m0) {
  l0 <- as.numeric(logLik(m0))
  lm <- as.numeric(logLik(object))
  return(1 - (lm / l0))
}
