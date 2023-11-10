#' Main vibe function
#' @export
#' @param object An object included in \code{vibe:::supported_classes}
#' @param ... Other arguments passed on to vibe methods.
vibe <- function(object, ...) {
  UseMethod("vibe", object)
}
