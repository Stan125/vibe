#' @importFrom stats logLik
#' @export
#' @method gof default
#' @importFrom methods is

gof.default <- function(object, gofmetric = "R2e", m0 = NULL, ...) {
  if (!is(object, c("glm", "gam", "gamlss")))
  if (gofmetric == "R2e" & is.null(m0))
    stop("Empty model needs to be provided")

  if (gofmetric == "R2e") {
    l0 <- as.numeric(logLik(m0))
    lm <- as.numeric(logLik(object))
    n <- length(object$y)
    return(1 - (lm / l0)^(-(2 / n) * l0))
  }
}

#' Function to obtain goodness-of-fit from regression object
#' @param object Regression object. One of \code{vibe:::supported_classes}.
#' @param ... Additional arguments
#' @export
gof <- function(object, ...)
  UseMethod("gof", object)
