#' Internal function
#'
#' @param X ...
#' @param groups_X ...
#' @param prior_X ...
#'
transform_X <- function(X, groups_X, prior_X){
  p <- dim(X)[2]
  n <- dim(X)[1]

  M <- matrix(1,n,p)
  for(i in 1:length(groups_X)){
    IDs_B <- groups_X[[i]]
    prior <- prior_X[[i]]

    if (prior == "A_exp"){
      M[, IDs_B] <- 1/length(IDs_B)*M[, IDs_B]
    } else if (prior == "A"){
      M[, IDs_B] <- M[, IDs_B]
    } else if (prior == "B"){
      M[, IDs_B] <- M[, IDs_B]
    }
  }
  return(M)
}
