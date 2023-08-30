#' Combine cont-cont (B), cont-disc (Rho) & disc-disc (Phi) couplings to an
#' adjacency matrix for a specified fit
#'
#' @param fit fit calculated in fit_MGM() or fit_MGM_par()
#' @param lambda select a lambda id calculated in fit
#'
#' @export
get_adj_mat <- function(fit, lambda){

  B <- fit$fit[[lambda]]$B
  Rho <- fit$fit[[lambda]]$Rho
  Phi <- fit$fit[[lambda]]$Phi

  Rho <- baseline_transform_Rho(Rho, fit$input_par$Y_levels) # new: 10.02.23
  Phi <- baseline_transform_Phi(Phi, fit$input_par$Y_levels) # new: 10.02.23

  adj_mat <- rbind(cbind(B,t(Rho)), cbind(Rho,Phi))
  adj_mat_diag <- c(diag(adj_mat)[1:ncol(B)], rep(-1, ncol(Phi)))
  diag(adj_mat) <- 0
  adj_mat <- adj_mat + t(adj_mat)
  diag(adj_mat) <- adj_mat_diag
  dim(adj_mat)

  colnames(adj_mat) <- c(fit$input_par$X_feature_names, colnames(fit$input_par$Y))
  rownames(adj_mat) <- colnames(adj_mat)

  Yids_del <- cumsum(fit$input_par$Y_levels)- (fit$input_par$Y_levels-1)
  # Y_del <- Yids[which(fit$input_par$Y_levels == 2)] - 1 # first appearing col! or second? -> IPI_AGE_0 or IPI_AGE_1
  # Y_del <- fit$input_par$Y_levels

  adj_mat <- adj_mat[-(ncol(B) + Yids_del), -(ncol(B) + Yids_del)]

  ## set diag to 0
  diag(adj_mat) <- 0
  
  return(adj_mat)
}
