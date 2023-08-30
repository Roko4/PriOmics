#' Internal function
#'
#' @param input_list  ...
#'
B_Rho_Phi_alphap_alphaq <- function(input_list){
  B <- input_list[[1]]
  Rho <- input_list[[2]]
  Phi <- input_list[[3]]
  alphap <- input_list[[4]]
  alphaq <- input_list[[5]]

  p <- nrow(B)
  q <- nrow(Phi)
  sizes <- cumsum(c(p*p, p*q, q*q, p, q))
  x <- rep(0, sizes[5])

  x[1:sizes[1]] <- as.vector(t(B))
  x[(sizes[1]+1):sizes[2]] <- as.vector(t(Rho))
  x[(sizes[2]+1):sizes[3]] <- as.vector(t(Phi))
  x[(sizes[3]+1):sizes[4]] <- alphap
  x[(sizes[4]+1):sizes[5]] <- alphaq
  return(x)
}
