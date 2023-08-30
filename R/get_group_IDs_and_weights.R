#' Internal function
#'
#' @param IDs_B ...
#' @param IDs_rho ...
#' @param X ...
#' @param Y ...
#' @param Y_levels ...
#'
get_group_IDs_and_weights <- function(IDs_B, IDs_rho, X, Y, Y_levels){
  p <- ncol(X)
  q <- sum(Y_levels)
  sds <- matrix(rep(1, p), ncol = 1)
  ps <- matrix(apply(Y, 2, mean), ncol = 1)
  ps <- sqrt(ps*(1-ps))
  Rho_cov <- matrix(1, q, p)
  Phi_cov <- matrix(1, q, q)
  Rho_cov <- Rho_cov * (ps %*% t(sds))
  Phi_cov  <- Phi_cov * (ps %*% t(ps))

  ### B
  B <- matrix(1:(p*p), p, p, byrow = T)
  B[lower.tri(B)] <- 0
  diag(B) <- 0

  pairs_B <- list()
  k = 1
  for (i in 1:length(IDs_B)){
    for (j in 1:length(IDs_B)){
      temp <- list(IDs_B[[i]], IDs_B[[j]])
      pairs_B[[k]] <- temp
      k = k+1
    }
  }
  Blist = as.list(rep(0, length(pairs_B)))
  Blist_weights = as.list(rep(0, length(pairs_B)))

  j = 1
  for (i in 1:length(pairs_B)) {
    B0 <- as.vector(t(B[pairs_B[[i]][[1]], pairs_B[[i]][[2]]]))
    B0 <- B0[B0 != 0]
    if (length(B0) > 0) {
      Blist[[j]] = B0
      Blist_weights[[j]] = sqrt(length(B0)) #only valid if X is standardized
      j = j + 1
    }
  }

  ### Rho
  rho <- matrix(1:(p*q), q, p, byrow = T)
  pairs_rho <- list()
  k = 1
  for (i in 1:length(IDs_rho)){
    for (j in 1:length(IDs_B)){
      temp <- list(IDs_rho[[i]], IDs_B[[j]])
      pairs_rho[[k]] <- temp
      k = k+1
    }
  }
  rholist = as.list(rep(0, length(pairs_rho)))
  rholist_weights = as.list(rep(0, length(pairs_rho)))

  j = 1
  for (i in 1:length(pairs_rho)) {
    rholist[[i]] <- p*p + as.vector(t(rho[pairs_rho[[i]][[1]], pairs_rho[[i]][[2]]]))
    rholist_weights[[i]] <- sqrt(sum(c(Rho_cov[pairs_rho[[i]][[1]], pairs_rho[[i]][[2]]])^2))
  }

  ### Phi
  phi <- matrix(1:(q*q), q, q, byrow = T)
  q0 <- length(IDs_rho)*((length(IDs_rho)-1)/2)
  philist = as.list(rep(0, q0))
  philist_weights = as.list(rep(0, q0))

  k = 1
  for (i in 1:length(IDs_rho)) {
    for (j in i:(length(IDs_rho)-1)) {
      if (i == length(IDs_rho) && j == length(IDs_rho)) {
        break
      } else {
        philist[[k]] = p*p + q*p + as.vector(t(phi[IDs_rho[[i]], IDs_rho[[j+1]]]))
        # print(p*p + q*p + as.vector(t(phi[IDs_rho[[i]], IDs_rho[[j+1]]])))
        philist_weights[[k]] = sqrt(sum(Phi_cov[IDs_rho[[i]], IDs_rho[[j+1]]]^2))
        k = k + 1
      }
    }
  }

  groups <- c(Blist, rholist, philist)
  weights <- c(Blist_weights, rholist_weights, philist_weights)

  group_IDs <- list("groups" = groups, "weights" = weights)

  return(group_IDs)
}
