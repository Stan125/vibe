#' Parallel Column Apply
#'
#' works like apply but also coerces into nice format like sapply. Gives out a single vector of numbers
#'
#' @keywords internal
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom parallel mclapply
pcapply <- function(X, FUN, ncores, progress = TRUE) {
  # Coerce to list where each element is one row
  li <- unname(as.list(as.data.frame(t(X))))

  # Apply function
  if (progress)
    output <- pbmclapply(li, FUN = FUN, ignore.interactive = TRUE,
                         mc.cores = ncores)
  else
    output <- mclapply(li, FUN = FUN, mc.cores = ncores)

  # Make output nice
  if (is(output[[1]], "vector"))
    output <- unlist(output)

  # Return
  return(output)
}

#' Error handlers
#'
#' @keywords internal
error_handling <- function(object = NULL,
                           metric = NULL,
                           gofmetric = NULL,
                           progress = NULL) {

  # Object
  if (!is.null(object))
    stopifnot(any(class(object) %in% supported_classes))

  # Metric
  if (!is.null(metric))
    stopifnot(metric %in% c("relweights", "hp"))

  # gofmetric
  # stop if not in any of the supported metrics

  # Progress
  if (!is.null(progress))
    stopifnot(is.logical(progress))
}
