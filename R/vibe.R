#' vibe: Variable Importance BEyond linear models
#'
#' This is a very cool package. So hot right now.
#'
#' @docType package
#' @useDynLib vibe
#' @importFrom Rcpp sourceCpp
#' @name vibe
NULL

#' Main vibe function
#' @export
#' @param object An object included in \code{vibe:::supported_classes}
#' @param ... Additional arguments passed on to methods
vibe <- function(object, ...) {
  UseMethod("vibe", object)
}
