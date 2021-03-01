#' Unique combinations of variables
#'
#' Function to get all possible combinations for a set of covariates.
#'
#' This function takes a number (\code{k}) and then outputs a matrix with every
#' row depicting one unique combination of covariates. This function is used for
#' model fitting.
#'
#' @param k Number of covariates (or covariate groups).
#'
#' @importFrom gtools combinations
#' @export

acc <- function(k) {
  ## Combinations
  combs <- as.list(1:k)
  combs <- lapply(combs, FUN = function(x)
    return(matrix(0, nrow = n_combs(k, x), ncol = k)))
  for (i in 1:length(combs)) {
    temp_comb <- combinations(k, i)
    dtc <- dim(temp_comb)
    combs[[i]][1:dtc[1], 1:dtc[2]] <- temp_comb
  }
  combs <- do.call(rbind, combs)

  ## Binary combinations
  indices <- cbind(1:nrow(combs), as.vector(combs))
  indices <- indices[indices[, 2] > 0, ]
  binary <- matrix(0, nrow = nrow(combs), ncol = ncol(combs))
  binary[indices] <- 1

  return(list(combs = combs, binary = binary))
}

#' Internal function: Calculate number of combinations
#'
#'@keywords internal

n_combs <- function(n, r)
  return(factorial(n) / (factorial(n - r) * factorial(r)))

#' Obtain model ids given a number of covariates
#'
#' @keywords internal
mids <- function(ncov) {

  # Get all combinations
  combins <- acc(k = ncov)$combs

  # Name models
  model_ids <- apply(combins, MARGIN = 1, FUN = function(x)
    return(as.character(x[x > 0])))
  model_ids <- sapply(model_ids, FUN = function(x)
    return(do.call(paste0, as.list(c("x", x)))))
  model_ids <- c("x0", model_ids)

  # Return
  return(model_ids)
}
