#' Internal function
#'
#' @param input_list ...
#' @param X ...
#' @param Y ...
#' @param Y_levels ...
#' @param groups_X ...
#' @param prior_X ...
#' @param use_prior ...
#'
neglogli <- function(input_list, X, Y, Y_levels, groups_X, prior_X, use_prior) {
  B <- input_list[[1]]
  Rho <- input_list[[2]]
  Phi <- input_list[[3]]
  alphap <- input_list[[4]]
  alphaq <- input_list[[5]]

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

  # B:
  Bd <- diag(B)
  B <- B - diag(Bd)
  B[lower.tri(B)] <- 0
  B <- B + t(B)

  YRho <- Y %*% Rho
  YRho <- YRho %*% diag(rep(1, p)/Bd)
  XB <- X %*% B
  XB <- XB %*% diag(rep(1, p)/Bd)
  Xt <- t(X)
  RhoX <- Rho %*% Xt

  # Phi:
  Phi <- Phi - diag(diag(Phi))
  Phi[lower.tri(Phi)] <- 0
  Phi <- Phi + t(Phi)
  Phirr <- t(matrix(rep(alphaq, n), nrow = n, byrow = T))
  PhiY <- Phi %*% t(Y)
  levelSum <- c(1, Y_levels)
  levelSum <- cumsum(levelSum)
  consts <- matrix(rep(alphap, n), nrow = n, byrow = T)

  PLcont1 <- (-(n/2))*sum(log(-Bd)) # here, the constant term is neglected
  PLcont2 <- consts + YRho + XB + X_response
  PLcont2 <- PLcont2 %*% diag(sqrt(-Bd))
  PLcont2 <- PLcont2*PLcont2
  PLcont2 <- 0.5*sum(apply(PLcont2, 2, sum))
  temp <- RhoX + Phirr + PhiY

  PLdisc <- 0
  for (r in 1:q){
    temp2 <- temp[levelSum[r]:(levelSum[r]+Y_levels[r]-1), ]
    denominator <- apply(exp(diag(Y_levels[r]) %*% temp2), 2, sum)
    numerator <- apply(Y[, levelSum[r]:(levelSum[r]+Y_levels[r]-1)]*t(temp2), 1, sum)
    PLdisc <- PLdisc-numerator+log(denominator)
  }
  PLdisc <- sum(PLdisc)

  return((PLcont1+PLcont2+PLdisc)/n)
}
