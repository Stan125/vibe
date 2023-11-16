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
  if (any(mcee == "glm") && any(mcee == "lm") && !any(mcee == "gam")) {
    mcee <- "glm"
  }
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

#' @title Is this combination of class/gof/varimp supported by [vibe]?
#' @description This function exists to check that all arguments are properly
#'   specified around most functions in the package, and if they are, whether
#'   the object/gof/varimp is supported.
#'
#' @keywords internal
args_supported <- function(object = NULL,
                           varimp = NULL,
                           gof = NULL,
                           progress = NULL) {
  if (!is.null(object)) {
    if (!is_this_class_supported(object)) {
      stop("This class is not supported")
    } else {
      object_class <- class_finder(object)
    }
  }

  if (!is.null(varimp)) {
    if (!is_varimp_supported(varimp)) {
      stop(paste0("Variable importance metric ", varimp, " is not supported"))
    }
  }

  if (!is.null(gof)) {
    if (!is_gof_supported(gof)) {
      stop(paste0("Goodness-of-fit measure ", gof, " is not supported"))
    }
  }

  if (!is.null(progress)) {
    stopifnot(is.logical(progress))
  }

  if (!is.null(object) & !is.null(gof) & !is.null(varimp)) {
    combination_supported <-
      scam[
        scam$supported_classes == object_class &
          scam$varimp_measure == varimp &
          scam$gof_metric == gof,
        "implemented"
      ]

    if (!combination_supported) {
      stop(paste0(
        "This combination of\n",
        "Class: ", object_class, "\n",
        "Variable importance metric: ", varimp, "\n",
        "Goodness-of-fit: ", gof, "\n",
        "is not supported. Please file an issue on the GitHub page."
      ))
    }
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

#' Supported class checker
#'
#' @keywords internal
is_this_class_supported <- function(object) {
  is_any_multiple_classes(object, levels(scam$supported_classes))
}

#' Supported varimp checker
#'
#' @keywords internal
is_varimp_supported <- function(varimp) {
  return(varimp %in% levels(scam$varimp_measure))
}

#' Supported gof checker
#'
#' @keywords internal
is_gof_supported <- function(gof) {
  return(gof %in% levels(scam$gof_metric))
}
