#' Internal function
#'
#' @param B continuous-continuous couplings (B)
#' @param Rho continuous-discrete couplings (Rho)
#' @param Phi discrete-discrete couplings (Phi)
#'
get_adj_mat2 <- function(B, Rho, Phi){

  adj_mat <- rbind(cbind(B,t(Rho)), cbind(Rho,Phi))
  adj_mat_diag <- c(diag(adj_mat)[1:ncol(B)], rep(-1, ncol(Phi)))
  diag(adj_mat) <- 0
  adj_mat <- adj_mat + t(adj_mat)
  diag(adj_mat) <- adj_mat_diag

  return(adj_mat)
}
