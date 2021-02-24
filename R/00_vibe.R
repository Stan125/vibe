#' vibe: Variable Importance BEyond linear models
#'
#' This is a very cool package. So hot right now.
#'
#' @section Functions:
#'
#' @docType package
#' @useDynLib vibe
#' @importFrom Rcpp sourceCpp
#' @name vibe
NULL

#' @export
vibe <- function(x, ...)
  UseMethod("vibe", x)
