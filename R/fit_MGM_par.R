#' Parallel calculation of a mixed graphical model with or without additional
#' priors (PriOmics algorithm)
#'
#' @param X continuous data (n x p matrix or data.frame)
#' @param Y  discrete/categorical data (n x p matrix or data.frame)
#' @param groups_X list of groups for prior assumption (for continuous data)
#' @param lambda_seq list of tuning parameter lambda (model selection)
#' @param iterations maximum number of iterations, until optimization is stopped
#' @param eps precision parameter
#' @param prior_X vector of prior assumption ("A", "B" or "A_exp" (not recommended)) for each group specified in "groups_X"
#' @param n_cor number of CPU cores
#'
#' @return list of resulting models according to lambda value
#'
#' @export
fit_MGM_par <- function(X, Y, groups_X = FALSE, lambda_seq, iterations = 100, eps = 1e-6, prior_X = FALSE, n_cor = 12){

  # if(missing(prior_X)){
  #   prior_X <- NA
  # }

  lambda_seq_list <- lambda_seq
  snowfall::sfInit(parallel = TRUE, cpus = n_cor, type = "SOCK") # set number of cpu cores
  snowfall::sfExportAll()
  fit <- snowfall::sfLapply(lambda_seq_list, function(lam_seq) fit_MGM(X, Y, groups_X, lam_seq, iterations, eps = 1e-6, prior_X))
  snowfall::sfStop( nostop=FALSE )

  fit_merged <- fit[[1]]
  for (i in 2:length(fit)){
    fit_merged <- Map(c, fit_merged, fit[[i]])
  }
  fit_merged$input_par <- fit_merged$input_par[1:11]

  return(fit_merged)
}
