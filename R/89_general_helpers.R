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
