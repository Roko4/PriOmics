#' Internal function
#'
#' @param x ...
#' @param X ...
#' @param Y ...
#' @param Y_levels ...
#' @param groups_X ...
#' @param prior_X  ...
#' @param use_prior ...
#'
BIC_function <- function(x, X, Y, Y_levels, groups_X, prior_X, use_prior) {
  # x = x_list[[1]]
  n = nrow(X)
  adj <- get_adj_mat2(x[[1]], x[[2]], x[[3]])
  gamma0 = 0.5

  NLL <- neglogli(x, X, Y, Y_levels, groups_X, prior_X, use_prior) * n

  ### remove Off-Diag entries from adj according to groups_X (necessary for
  ### Prior C to get accurate number of active parameter)
  # j = 1
  # for (i in 1:length(groups_X)){
  #   grp <- groups_X[[i]]
  #   grp_len <- length(grp)
  #   if (grp_len > 1){
  #     adj[j:(j+(grp_len-1)), j:(j+(grp_len-1))] <- 0
  #   }
  #   j = j+grp_len
  # }

  active_par <- sum(adj[upper.tri(adj, diag = FALSE)] != 0)

  EBIC <- 2*NLL + active_par*log(n) + 4*active_par*gamma0*log(ncol(adj))
  BIC <- 2*NLL + active_par*log(n)
  return(list("EBIC" = EBIC, "NLL" = NLL, "active_par" = active_par, "BIC" = BIC))
}
