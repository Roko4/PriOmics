#' Internal function
#'
#' @param list_Inv_B_Rho_Phi_alphap_alphaq ...
#' @param X ...
#' @param Y ...
#' @param Y_levels ...
#' @param groups_X ...
#' @param prior_X ...
#' @param use_prior ...
#'
grad_neglogli_R <- function(list_Inv_B_Rho_Phi_alphap_alphaq, X, Y, Y_levels, groups_X, prior_X, use_prior){
  B <- list_Inv_B_Rho_Phi_alphap_alphaq[[1]]
  Rho <- list_Inv_B_Rho_Phi_alphap_alphaq[[2]]
  Phi <- list_Inv_B_Rho_Phi_alphap_alphaq[[3]]
  alphap <- list_Inv_B_Rho_Phi_alphap_alphaq[[4]]
  alphaq <- list_Inv_B_Rho_Phi_alphap_alphaq[[5]]

  n <- dim(X)[1]
  p <- dim(B)[1]
  q <- length(Y_levels)

  if (use_prior == TRUE){
    M <- transform_X(X, groups_X, prior_X)
  } else {
    M <- matrix(rep(1, n*p), ncol = p)
  }

  X_response = X
  X <- M * X

  levelSum <- c(1, Y_levels)
  levelSum <- cumsum(levelSum)

  for (r in 1:q){
    Phi[levelSum[r]:(levelSum[r]+Y_levels[r]-1),
        levelSum[r]:(levelSum[r]+Y_levels[r]-1)] = 0
  }

  Bd <- diag(B)
  B <- B - diag(Bd)
  B[lower.tri(B)] <- 0
  B <- B + t(B)

  YRho <- Y %*% Rho
  YRho <- YRho %*% diag(rep(1, ncol(B)) / Bd)

  XB <- X %*% B
  XB <- XB %*% diag(rep(1, ncol(B)) / Bd)
  consts <- matrix(rep(alphap, n), nrow = n, byrow = T)

  res <- consts + YRho + XB + X_response

  Xt <- t(X)
  gradBd <- rep(0, p)
  for (s in 1:p){
    gradBd[s] = - n/(2*Bd[s]) - 0.5 * (res[, s] %*% res[, s]) + (res[, s] %*% (XB[, s] + YRho[, s]))
  }

  gradB <- - Xt %*% res
  gradB <- gradB - diag(diag(gradB))

  gradB_low <- gradB
  gradB_low[upper.tri(gradB)] <- 0
  gradB_up <- gradB
  gradB_up[lower.tri(gradB)] <- 0
  gradB <- t(gradB_low) + gradB_up

  gradalphap <- - diag(Bd) %*% apply(res, 2, sum)
  gradalphap <- as.vector(gradalphap)

  gradRho <- - (t(Y) %*% res)

  Xt <- t(X)
  RhoX <- Rho %*% Xt
  Phi <- Phi - diag(diag(Phi))
  Phi[lower.tri(Phi)] <- 0
  Phi <- Phi + t(Phi)
  Phirr <- t(matrix(rep(alphaq, n), nrow = n, byrow = T))
  PhiY <- Phi %*% t(Y)
  discprod <- t(RhoX + Phirr + PhiY)

  for (r in 1:q){
    disctemp <- discprod[, levelSum[r]:(levelSum[r]+Y_levels[r]-1)]

    denominator <- matrixStats::rowLogSumExps(disctemp)

    # denominator <- log(apply(exp(disctemp), 1, sum))
    disctemp <- disctemp - denominator
    disctemp <- exp(disctemp)
    temp <- disctemp - Y[, levelSum[r]:(levelSum[r]+Y_levels[r]-1)]
    discprod[, levelSum[r]:(levelSum[r]+Y_levels[r]-1)] <- temp
  }

  gradalphaq <- apply(discprod, 2, sum)
  gradw <- Xt %*% discprod
  gradRho <- gradRho + t(gradw)
  gradPhi <- t(Y) %*% discprod

  for (r in 1:q){
    gradPhi[levelSum[r]:(levelSum[r]+Y_levels[r]-1),
            levelSum[r]:(levelSum[r]+Y_levels[r]-1)] = 0
  }

  gradPhi_low <- gradPhi
  gradPhi_low[upper.tri(gradPhi)] <- 0
  gradPhi_up <- gradPhi
  gradPhi_up[lower.tri(gradPhi)] <- 0
  gradPhi <- t(gradPhi_low) + gradPhi_up

  if (use_prior == TRUE){
    gradB <- transform_B(gradB, groups_X, prior_X)
  }

  diag(gradB) <- gradBd
  gradB <- gradB/n
  gradRho <- gradRho/n
  gradPhi <- gradPhi/n
  gradalphap <- gradalphap/n
  gradalphaq <- gradalphaq/n
  grad_list <- list("gradB" = gradB, "gradRho" = gradRho,
                    "gradPhi" = gradPhi, "gradalphap" = gradalphap,
                    "gradalphaq" = gradalphaq)
  #print(grad_list[[1]][1:5, 1:5])
  return(grad_list)
}

