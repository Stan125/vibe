#' @importFrom stats logLik
#' @export
#' @method gof default

gof.default <- function(object, gofmetric = "R2e", m0 = NULL) {
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

#' @export
gof <- function(x, ...)
  UseMethod("gof", x)
