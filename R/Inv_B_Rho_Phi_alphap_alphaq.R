#' Internal function
#'
#' @param x ...
#' @param p ...
#' @param q ...
#'
Inv_B_Rho_Phi_alphap_alphaq <- function(x, p, q){
  sizes <- cumsum(c(p*p, p*q, q*q, p, q))
  B <- matrix(x[1:(sizes[1])], p, p, byrow = T)
  Rho <- matrix(x[(sizes[1]+1):sizes[2]], q, p, byrow = T)
  Phi <- matrix(x[(sizes[2]+1):sizes[3]], q, q, byrow = T)
  alphap <- x[(sizes[3]+1):sizes[4]]
  alphaq <- x[(sizes[4]+1):sizes[5]]

  list_Inv_B_Rho_Phi_alphap_alphaq <- list("B" = B, "Rho" = Rho, "Phi" = Phi, "alphap" = alphap, "alphaq" = alphaq)
  return(list_Inv_B_Rho_Phi_alphap_alphaq)
}
