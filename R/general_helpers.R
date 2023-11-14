#' Parallel Column Apply
#'
#' works like apply but also coerces into nice format like sapply.
#' Gives out a single vector of numbers
#'
#' @keywords internal
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom parallel mclapply
#' @importFrom methods is
pcapply <- function(x_mat, fun, ncores, progress = TRUE) {
  # Coerce to list where each element is one row
  li <- unname(as.list(as.data.frame(t(x_mat))))

  # Apply function
  if (progress) {
    output <- pbmclapply(li,
      FUN = fun, ignore.interactive = TRUE,
      mc.cores = ncores
    )
  } else {
    output <- mclapply(li, FUN = fun, mc.cores = ncores)
  }

  # Make output nice
  if (is(output[[1]], "vector")) {
    output <- unlist(output)
  }

  # Return
  return(output)
}

#' Class finder
#'
#' This function is just there to find the class of object
#' and only give back one class...
#' @keywords internal
class_finder <- function(object) {
  mcee <- levels(scam$supported_classes)[
    levels(scam$supported_classes) %in% class(object)
  ]
  if (any(mcee == "gam") && !any(mcee == "gamlss")) {
    mcee <- "gam"
  }
  if (any(mcee == "gamlss")) {
    mcee <- "gamlss"
  }
  return(mcee)
}

#' Number of modeled parameters determiner
#'
#' Determines the number of modeled parameters in gamlss models
#' @keywords internal
det_npar <- function(object) {
  pars <- object$parameters
  modeled_pars <- sapply(pars, FUN = function(x) {
    if (ncol(object[[paste0(x, ".x")]]) > 1) {
      return(x)
    } else {
      return(NULL)
    }
  })
  return(unlist(modeled_pars))
}

#' Error handlers
#'
#' @keywords internal
error_handling <- function(object = NULL,
                           varimp = NULL,
                           gof = NULL,
                           progress = NULL) {
  # Object
  if (!is.null(object)) {
    stopifnot(any(class(object) %in% levels(scam$supported_classes)))
  }

  # Metric
  if (!is.null(varimp)) {
    stopifnot(varimp %in% c("relweights", "hp"))
  }

  # gofmetric
  # stop if not in any of the supported metrics

  # Progress
  if (!is.null(progress)) {
    stopifnot(is.logical(progress))
  }
}

#' Multiple class checker
#'
#' @keywords internal
#' @noRd
#' @importFrom methods is

is_any_multiple_classes <- function(object, classes) {
  isin <- sapply(classes, FUN = function(x) is(object, x))
  return(any(isin))
}