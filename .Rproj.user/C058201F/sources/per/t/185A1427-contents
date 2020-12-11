#' @importFrom stats logLik

gof.default <- function(object, gofmetric = "R2e", m0 = NULL) {
  if (!is(object, c("glm", "gam", "gamlss")))
  if (gof == "R2e" & is.null(m0))
    stop("Empty model needs to be provided")

  if (gof == "R2e") {
    l0 <- logLik(m0)
    lm <- logLik(object)
    return(1 - (lm / l0)^(-(2 / n) * l0))
  }
}
