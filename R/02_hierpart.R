#' Step Four: Do hierarchical partitioning
#'
#' @keywords internal
#' @importFrom tibble add_column

part <- function(gofs_list) {

  # Partition for first param
  res_mu <- part_core(gofs_vector = gofs_list$gofs$mu,
                      expl_names = gofs_list$expl_names$mu,
                      model_ids = names(gofs_list$gofs$mu))
  part_mu <- res_mu$main
  part_mu <- add_column(part_mu, param = "mu", .after = 1)

  # If available partition for second param
  if (gofs_list$npar == 2) {
    res_sigma <- part_core(gofs_list$gofs$sigma, gofs_list$expl_names$sigma,
                           names(gofs_list$gofs$sigma))
    part_sigma <- res_sigma$main
    part_sigma <- add_column(part_sigma, param = "sigma", .after = 1)
  }

  # Assemble results and joint_results dataframe
  if (gofs_list$npar == 1) {
    results <- part_mu
    joint_res <- res_mu$joint_allocs
  }
  if (gofs_list$npar == 2) {
    results <- rbind(part_mu, part_sigma)
    joint_res <- list(res_mu = res_mu$joint_allocs,
                      res_sigma = res_sigma$joint_allocs)
  }

  # Assemble part class
  part <- list(results = results, npar = gofs_list$npar,
               method = gofs_list$method, gof = gofs_list$gof,
               joint_results = joint_res)

  # Return the class object
  return(part)
}

#' Core function of part(). Does the partitioning.
#'
#' @importFrom combinat permn
#' @importFrom tibble tibble as_tibble
#' @keywords internal

part_core <- function(gofs_vector, expl_names, model_ids) {
  # How many variables/groups?
  nvar <- length(expl_names)

  # Get all permutations and sort them
  perms <- do.call(rbind, permn(nvar)) # Every row is a permutation
  first_two_nums <- apply(perms, 1, FUN = function(x) return((x[1] * 10) + x[2]))
  sort_order <- sort(first_two_nums, index.return = TRUE)$ix
  perms <- perms[sort_order, ]

  # Differences for each permutation and each modeled parameter
  # Heart of the function!
  results_list <- perm_cpp(perms, model_ids, gofs_vector)

  # Put results in right places
  I <- results_list$I
  J <- results_list$J
  J_allocs <- results_list$J_allocs

  # Make J_allocs pretty
  colnames(J_allocs) <- expl_names
  row.names(J_allocs) <- expl_names
  J_allocs <- cbind.data.frame(var = expl_names, J_allocs)

  # Put J_alloc_perc to J_alloc (Percentages)
  J_allocs_perc <- apply(J_allocs[, -1], 2, FUN = function(x) return(x / sum(x))) # -1 because first column is variable names
  colnames(J_allocs_perc) <- paste0(colnames(J_allocs_perc), "_perc")
  J_allocs <- cbind(J_allocs, J_allocs_perc)

  # Make the sum and convert to tibble for nice printing
  J_allocs <-
    rbind(J_allocs, data.frame(var = "SUM", t(apply(J_allocs[,-1], 2, FUN = sum))))
  J_allocs <- as_tibble(J_allocs)

  # Get total effects
  Tot <- gofs_vector[2:(nvar+1)] - gofs_vector[1]

  # Create and return tibble
  main_res <- tibble(var = expl_names, indep_effects = I,
                     joint_effects = J, total_effects = Tot,
                     indep_perc = I / sum(I), joint_perc = J / sum(J))
  return(list(main = main_res, joint_allocs = J_allocs))
}
