#' Main vibe function
#' @export
#' @param object An object included in \code{vibe:::supported_classes}
vibe <- function(object) {
  UseMethod("vibe", object)
}
