#' Internal function
#'
#' @param X ...
#' @param Y ...
#'
make_starting_parameters <- function(X, Y){
  p <- ncol(X)
  q <- ncol(Y)
  B <- -diag(rep(1, p))
  Rho <- matrix(0, q, p, byrow = T)
  Phi <- matrix(0, q, q, byrow = T)
  alphap <- rep(0, p)
  alphaq <- rep(0, q)

  starting_parameters <- list("B" = B, "Rho" = Rho, "Phi" = Phi, "alphap" = alphap, "alphaq" = alphaq, "p" = p, "q" = q)
  return(starting_parameters)
}
