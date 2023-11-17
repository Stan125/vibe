#' @title Fit a null model (model without any explanatory variables)
#'
#' @param object Model object, one of the ones supported in [scam]
#' @param ... Other arguments
#'
#' @return a fitted model object
#' @export
#'
fit_null_model <- function(object, ...) {
  UseMethod("fit_mean_model", x)
}

#' @importFrom stats family glm
#' @rdname fit_null_model
fit_null_model.glm <- function(object, ...) {
  depvar <- object$y
  fam <- family(object)
  mod <- glm(depvar ~ ., family = fam)
  return(mod)
}

#' @importFrom stats family
#' @importFrom mgcv gam
#' @rdname fit_null_model
fit_null_model.gam <- function(object, ...) {
  depvar <- object$y
  fam <- family(object)
  mod <- gam(depvar ~ ., family = fam)
  return(mod)
}

#' @importFrom stats family
#' @importFrom gamlss gamlss
#' @rdname fit_null_model
fit_null_model.gamlss <- function(object, ...) {
  depvar <- object$y
  fam <- family(object)
  mod <- gamlss(depvar ~ ., family = fam, trace = FALSE)
  return(mod)
}
